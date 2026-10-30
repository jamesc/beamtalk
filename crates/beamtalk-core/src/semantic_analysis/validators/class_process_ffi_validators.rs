// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Class-method FFI lints: `class-process-ffi` (BT-4031) and its ADR 0142
//! inversion `class-method-process-resource` (BT-4302).
//!
//! **DDD Context:** Semantic Analysis
//!
//! A class method runs either in its class's `gen_server` process or, when
//! it runs in the caller, in the caller's process. The two lints split on the
//! one rule that decides it, [`ClassHierarchy::runs_class_method_in_caller`]
//! (shared with codegen's direct-call table and the `class-state-abroad`
//! lint), so a call gets at most one of them:
//!
//! - **`class-process-ffi`** — the class or an ancestor declares class
//!   variables, so the method runs in the class process, and it calls a function that depends on the
//!   *calling* process: `gen_tcp:accept/1` blocks the one shared class
//!   process, `gen_tcp:controlling_process/2` and `file:sendfile/5` fail with
//!   `not_owner` (the caller owns the socket), and `erlang:self/0`, the
//!   process dictionary, `link` and `monitor` all see the class process.
//!   Suppress deliberate serialisation with `@expect class_process_ffi`.
//! - **`class-method-process-resource`** — no class in the chain declares
//!   class variables, so the method runs in the caller, and it creates a resource tied to the calling
//!   process (`ets:new`, `open_port`, `spawn_link`, `link`, `monitor`,
//!   `register`, `erlang:send_after`/`start_timer`, the `timer` interval
//!   timers and `timer:send_after`) or acts on the calling process itself
//!   (`self()`, the process dictionary, `process_flag`, `unlink`,
//!   `demonitor`). The fix is an Actor that owns the resource, or
//!   `classState:` to keep the class process. Suppress with
//!   `@expect class_method_process_resource`.
//!
//! **The rule moves, the lints follow.** ADR 0129 direct-called a method only
//! when it was `class sealed` in a `sealed` class without class variables.
//! ADR 0142 drops the sealing requirement, so every class method of a class
//! without class variables in its chain runs in the caller; BT-4301 moved the
//! shared rule to ADR 0142's, and these lints follow it.
//!
//! **Fire condition** (both lints):
//!
//! 1. the method is class-side: in `class.class_methods`, or a standalone
//!    `Sub class >> m` definition (`ClassBuilder classMethods:` blocks are not
//!    inspected in v1);
//! 2. the class is known to the hierarchy and the selector is not
//!    process-bound ([`ClassInfo::is_process_bound_class_selector`]: those
//!    keep their routing whatever the class declares, so no hint can help);
//! 3. the body contains a direct `(Erlang mod) fn ...` call listed in
//!    [`CLASS_METHOD_FFI`] with an effect for the process the method runs in.
//!    Transitive calls (a helper does the FFI) are not followed in v1.
//!
//! **Deliberately excluded from the table** (flagging them would fire on the
//! blessed "FFI namespace" pattern): `file:open/read_file/write_file/close`
//! and every pure or query FFI function (`lists`, `maps`, `string`, ...);
//! `inet` entirely (a per-function audit is deferred); the `timer` functions
//! whose result is not tied to the caller (`timer:apply_after/4`, which
//! outlives it, `exit_after`/`kill_after`, which target rather than own it)
//! or that create nothing (`timer:seconds/1`, `timer:tc/1`, `timer:sleep/1`,
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
    /// The call creates a resource tied to the calling process (ETS table,
    /// port, linked process, link, monitor, registered name, timer).
    Resource,
    /// The call reads or changes the calling process itself: its pid, process
    /// dictionary, process flags, links or monitors.
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
/// dictionary, `process_flag`, links, monitors) has one row with both
/// effects. Modelled on
/// `KNOWN_ARITIES`: a spec-derived attribute is a possible follow-up. See the
/// module docs for what is deliberately left out.
const CLASS_METHOD_FFI: &[ClassMethodFfi] = &[
    row("erlang", "self", OWNERSHIP, IDENTITY),
    row("erlang", "send", OWNERSHIP, None),
    row("erlang", "get", OWNERSHIP, IDENTITY),
    row("erlang", "put", OWNERSHIP, IDENTITY),
    row("erlang", "erase", OWNERSHIP, IDENTITY),
    row("erlang", "process_flag", OWNERSHIP, IDENTITY),
    row("erlang", "link", OWNERSHIP, RESOURCE),
    row("erlang", "unlink", OWNERSHIP, IDENTITY),
    row("erlang", "monitor", OWNERSHIP, RESOURCE),
    row("erlang", "demonitor", OWNERSHIP, IDENTITY),
    row("erlang", "spawn_link", None, RESOURCE),
    row("erlang", "open_port", None, RESOURCE),
    row("erlang", "register", None, RESOURCE),
    row("erlang", "send_after", None, RESOURCE),
    row("erlang", "start_timer", None, RESOURCE),
    row("ets", "new", None, RESOURCE),
    row("timer", "apply_interval", None, RESOURCE),
    row("timer", "apply_repeatedly", None, RESOURCE),
    row("timer", "send_after", None, RESOURCE),
    row("timer", "send_interval", None, RESOURCE),
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
    // The one rule for where a class method runs (ADR 0142 §1: no class
    // variables in the chain, not process-bound, sealing irrelevant).
    let runs_in_caller = hierarchy.runs_class_method_in_caller(class_name, &selector);
    let class_process_hint = class_process_hint(class);

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
            let diagnostic = if runs_in_caller {
                entry.in_caller.map(|e| site.caller(e))
            } else {
                entry
                    .in_class_process
                    .map(|s| site.class_process(s, &class_process_hint))
            };
            diagnostics.extend(diagnostic);
        });
    }
}

/// The `class-process-ffi` hint for a class method that runs in the class
/// process. Under ADR 0142 that happens only for a non-process-bound selector
/// when the class or an ancestor declares class variables (BT-4301): the class
/// keeps its process whatever is sealed, so the call has to move.
fn class_process_hint(class: &ClassInfo) -> String {
    let class_name = class.name.as_str();
    if class.has_class_variables() {
        format!(
            "`{class_name}` declares class variables, so its class methods run in the class \
             process: move the call into an instance method or an Actor"
        )
    } else {
        format!(
            "an ancestor of `{class_name}` declares class variables, so its class methods run \
             in the class process: move the call into an instance method or an Actor"
        )
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
    /// `class-process-ffi`: the method runs in the class process (its chain
    /// declares class variables) and the call depends on the calling process.
    fn class_process(&self, sensitivity: Sensitivity, hint: &str) -> Diagnostic {
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
                "class method `{class_name} {selector}` calls {call}, {why} — but this method \
                 runs in the class's gen_server process, not the caller's, {consequence}"
            ),
            expr.span(),
        )
        .with_hint(hint.to_string())
        .with_category(DiagnosticCategory::ClassProcessFfi)
    }

    /// `class-method-process-resource`: the method runs in the caller (ADR
    /// 0142) and the call creates a resource tied to, or acts on, the calling
    /// process.
    fn caller(&self, effect: CallerEffect) -> Diagnostic {
        let Self {
            class_name,
            selector,
            call,
            expr,
        } = self;
        let (why, consequence) = match effect {
            CallerEffect::Resource => (
                "which creates a resource tied to the calling process",
                "so the resource is tied to the caller, not the class process",
            ),
            CallerEffect::Identity => (
                "which reads or changes the calling process itself (its pid, process \
                 dictionary, flags, links or monitors)",
                "so it acts on the caller, not the class process",
            ),
        };
        Diagnostic::warning(
            format!(
                "class method `{class_name} {selector}` calls {call}, {why} — this method runs \
                 in the caller's process, {consequence}"
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

    /// Where a probe class's method runs.
    #[derive(Clone, Copy, Debug)]
    enum Probe {
        /// `class sealed` in a `sealed` class without class variables: the
        /// caller (ADR 0129 already direct-called it).
        SealedStateless,
        /// An open class without class variables: the caller (ADR 0142 §1,
        /// BT-4301).
        OpenStateless,
        /// A class with class variables: the class process, always.
        Stateful,
        /// A stateless subclass of a class with class variables: the class
        /// process (ADR 0142 §1 rule 2 over the chain).
        InheritsState,
    }

    const IN_CALLER: [Probe; 2] = [Probe::SealedStateless, Probe::OpenStateless];
    const IN_CLASS_PROCESS: [Probe; 2] = [Probe::Stateful, Probe::InheritsState];

    /// A one-method class calling `(Erlang module) function: x` from
    /// `class probe: x`.
    fn probe_class(module: &str, function: &str, probe: Probe) -> String {
        let call = format!("(Erlang {module}) {function}: x");
        match probe {
            Probe::SealedStateless => {
                format!("sealed Object subclass: Probe\n  class sealed probe: x => {call}\n")
            }
            Probe::OpenStateless => format!("Object subclass: Probe\n  class probe: x => {call}\n"),
            Probe::Stateful => format!(
                "Object subclass: Probe\n  classState: n = 0\n\n  class probe: x => {call}\n"
            ),
            Probe::InheritsState => format!(
                "Object subclass: Base\n  classState: n = 0\n\n\
                 Base subclass: Probe\n  class probe: x => {call}\n"
            ),
        }
    }

    // ── table: one positive and one negative per row ─────────────────────

    #[test]
    fn every_caller_row_fires_in_the_caller_and_not_in_the_class_process() {
        for entry in CLASS_METHOD_FFI.iter().filter(|r| r.in_caller.is_some()) {
            let (m, f) = (entry.module, entry.function);
            for probe in IN_CALLER {
                let fires = in_caller(&probe_class(m, f, probe));
                assert_eq!(fires.len(), 1, "{m}:{f} {probe:?}: {fires:?}");
                assert!(
                    fires[0].message.contains(&format!("`{m}:{f}/1`")),
                    "{fires:?}"
                );
                assert_eq!(fires[0].severity, Severity::Warning);
            }
            for probe in IN_CLASS_PROCESS {
                let silent = in_caller(&probe_class(m, f, probe));
                assert!(silent.is_empty(), "{m}:{f} {probe:?}: {silent:?}");
            }
        }
    }

    #[test]
    fn rows_without_a_caller_effect_never_fire_class_method_process_resource() {
        for entry in CLASS_METHOD_FFI.iter().filter(|r| r.in_caller.is_none()) {
            let (m, f) = (entry.module, entry.function);
            for probe in IN_CALLER {
                let silent = in_caller(&probe_class(m, f, probe));
                assert!(silent.is_empty(), "{m}:{f} {probe:?}: {silent:?}");
            }
        }
    }

    #[test]
    fn every_class_process_row_fires_in_the_class_process_and_not_in_the_caller() {
        for entry in CLASS_METHOD_FFI
            .iter()
            .filter(|r| r.in_class_process.is_some())
        {
            let (m, f) = (entry.module, entry.function);
            for probe in IN_CLASS_PROCESS {
                let fires = in_class_process(&probe_class(m, f, probe));
                assert_eq!(fires.len(), 1, "{m}:{f} {probe:?}: {fires:?}");
                assert!(
                    fires[0].message.contains(&format!("`{m}:{f}/1`")),
                    "{fires:?}"
                );
            }
            for probe in IN_CALLER {
                let silent = in_class_process(&probe_class(m, f, probe));
                assert!(silent.is_empty(), "{m}:{f} {probe:?}: {silent:?}");
            }
        }
    }

    #[test]
    fn rows_without_a_class_process_effect_never_fire_class_process_ffi() {
        for entry in CLASS_METHOD_FFI
            .iter()
            .filter(|r| r.in_class_process.is_none())
        {
            let (m, f) = (entry.module, entry.function);
            for probe in IN_CLASS_PROCESS {
                let silent = in_class_process(&probe_class(m, f, probe));
                assert!(silent.is_empty(), "{m}:{f} {probe:?}: {silent:?}");
            }
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
    fn direct_called_resource_message_and_hint_name_the_fix() {
        let src = "sealed Object subclass: Tables
  class sealed make => (Erlang ets) new: #t with: #()
";
        let diags = in_caller(src);
        assert_eq!(diags.len(), 1, "{diags:?}");
        let msg = &diags[0].message;
        assert!(msg.contains("`Tables make`"), "{msg}");
        assert!(msg.contains("`ets:new/2`"), "{msg}");
        assert!(msg.contains("creates a resource tied to the calling process"));
        assert!(msg.contains("runs in the caller's process"));
        let h = hint(&diags[0]);
        assert!(h.contains("Actor"), "{h}");
        assert!(h.contains("`classState:` on `Tables`"), "{h}");
    }

    #[test]
    fn unary_erlang_self_when_direct_called_acts_on_the_caller() {
        let diags =
            in_caller("sealed Object subclass: SealedC\n  class sealed me => Erlang erlang self\n");
        assert_eq!(diags.len(), 1, "{diags:?}");
        assert!(diags[0].message.contains("`erlang:self/0`"));
        assert!(diags[0].message.contains("calling process itself"));
        assert!(diags[0].message.contains("acts on the caller"));
    }

    #[test]
    fn process_flag_when_direct_called_changes_the_callers_flags() {
        let src = "sealed Object subclass: Flags
  class sealed trap => (Erlang erlang) process_flag: #trap_exit with: true
";
        let diags = in_caller(src);
        assert_eq!(diags.len(), 1, "{diags:?}");
        assert!(diags[0].message.contains("`erlang:process_flag/2`"));
    }

    #[test]
    fn timer_functions_not_tied_to_the_caller_are_silent() {
        let src = "sealed Object subclass: Timers
  class sealed a: m => (Erlang timer) apply_after: 1 with: m with: #f with: #()
  class sealed k: p => (Erlang timer) kill_after: 1 with: p
  class sealed e: p => (Erlang timer) exit_after: 1 with: p with: #r
";
        assert!(in_caller(src).is_empty());
    }

    #[test]
    fn pure_ffi_and_pure_timer_helpers_are_silent() {
        for header in ["sealed Object subclass: Lists", "Object subclass: Lists"] {
            let src = format!(
                "{header}
  class sealed rev: xs => (Erlang lists) reverse: xs
  class sealed read: p => (Erlang file) read_file: p
  class sealed secs: n => (Erlang timer) seconds: n
  class sealed nap: n => (Erlang timer) sleep: n
"
            );
            assert!(in_caller(&src).is_empty(), "{header}");
            assert!(in_class_process(&src).is_empty(), "{header}");
        }
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
        assert!(in_class_process(src).is_empty());
        let stateful = "Object subclass: Ctor2
  classState: n = 0

  class new => Erlang erlang self
  class startLink => (Erlang gen_tcp) accept: 1
";
        assert!(in_class_process(stateful).is_empty());
    }

    #[test]
    fn instance_side_methods_are_silent() {
        let src = "sealed Object subclass: Inst\n  me => Erlang erlang self\n";
        assert!(in_caller(src).is_empty());
        let open = "Object subclass: Inst2\n  me => Erlang erlang self\n";
        assert!(in_class_process(open).is_empty());
    }

    // ── class-process-ffi ────────────────────────────────────────────────

    #[test]
    fn issue_repro_stateful_class_fires_for_each_method() {
        let src = "Object subclass: Tcp
  classState: listeners = 0

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
        assert!(diags[1].message.contains("`gen_tcp:controlling_process/2`"));
        assert!(diags[1].message.contains("ownership"));
        assert!(diags[2].message.contains("`file:sendfile/5`"));
        for d in &diags {
            assert_eq!(d.severity, Severity::Warning);
            assert!(hint(d).contains("`Tcp` declares class variables"), "{d:?}");
        }
        assert!(in_caller(src).is_empty());
    }

    #[test]
    fn open_stateless_class_runs_in_the_caller_whatever_is_sealed() {
        // ADR 0142 §1 (BT-4301): sealing plays no part.
        for src in [
            "Object subclass: OpenC\n  class sealed me => Erlang erlang self\n",
            "sealed Object subclass: SealedC\n  class me2 => Erlang erlang self\n",
            "Object subclass: OpenD\n  class me3 => Erlang erlang self\n",
        ] {
            assert!(in_class_process(src).is_empty(), "{src}");
            assert_eq!(in_caller(src).len(), 1, "{src}");
        }
    }

    #[test]
    fn inherited_class_state_hint_names_the_ancestor() {
        let src = "Object subclass: Base
  classState: n = 0

Base subclass: Sub
  class me => Erlang erlang self
";
        let diags = in_class_process(src);
        assert_eq!(diags.len(), 1, "{diags:?}");
        let h = hint(&diags[0]);
        assert!(
            h.contains("an ancestor of `Sub` declares class variables"),
            "{h}"
        );
        assert!(in_caller(src).is_empty());
    }

    #[test]
    fn class_with_class_state_fires_with_the_move_hint_even_when_sealed() {
        let src = "sealed Object subclass: Stateful
  classState: n = 0

  class sealed me => Erlang erlang self
";
        let diags = in_class_process(src);
        assert_eq!(diags.len(), 1, "{diags:?}");
        let h = hint(&diags[0]);
        assert!(h.contains("`Stateful` declares class variables"), "{h}");
        assert!(h.contains("instance method or an Actor"), "{h}");
        assert!(in_caller(src).is_empty());
    }

    #[test]
    fn standalone_class_method_definition_fires() {
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

    fn assert_suppressed(src: &str, category: DiagnosticCategory) {
        let diags = after_expect(src);
        assert!(
            !diags.iter().any(|d| d.category == Some(category)),
            "{diags:?}"
        );
        assert!(
            !diags.iter().any(|d| d.message.contains("stale @expect")),
            "the directive matched: {diags:?}"
        );
    }

    #[test]
    fn expect_class_process_ffi_suppresses_the_warning() {
        let src = "Object subclass: StatefulC
  classState: n = 0

  class me =>
    @expect class_process_ffi
    Erlang erlang self
";
        assert_suppressed(src, DiagnosticCategory::ClassProcessFfi);
    }

    #[test]
    fn expect_class_method_process_resource_suppresses_the_warning() {
        let src = "sealed Object subclass: SealedC
  class sealed me =>
    @expect class_method_process_resource
    Erlang erlang self
";
        assert_suppressed(src, DiagnosticCategory::ClassMethodProcessResource);
    }

    #[test]
    fn expect_class_process_ffi_on_a_method_run_in_the_caller_is_reported_unused() {
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
