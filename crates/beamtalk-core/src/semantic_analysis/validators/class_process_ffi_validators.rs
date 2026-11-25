// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Class-method FFI lint: `class-method-process-resource` (ADR 0142,
//! BT-4302).
//!
//! **DDD Context:** Semantic Analysis
//!
//! A class method runs in the caller's process unless its selector is
//! process-bound ([`ClassHierarchy::runs_class_method_in_caller`], the one
//! rule, shared with codegen's direct-call table). The lint fires when such a
//! method creates a resource tied to the calling process (`ets:new`,
//! `open_port`, `spawn_link`, `link`, `monitor`, `register`,
//! `erlang:send_after`/`start_timer`, the `timer` interval timers and
//! `timer:send_after`) or acts on the calling process itself (`self()`, the
//! process dictionary, `process_flag`, `unlink`, `demonitor`). The fix is an
//! Actor that owns the resource. Suppress with
//! `@expect class_method_process_resource`.
//!
//! **Fire condition**:
//!
//! 1. the method is class-side: in `class.class_methods`, or a standalone
//!    `Sub class >> m` definition (`ClassBuilder classMethods:` blocks are not
//!    inspected in v1);
//! 2. the class is known to the hierarchy and the selector is not
//!    process-bound ([`ClassInfo::is_process_bound_class_selector`]: those
//!    keep their routing whatever the class declares, so no hint can help);
//! 3. the body contains a direct `(Erlang mod) fn ...` call listed in
//!    [`CLASS_METHOD_FFI`] with an effect in the caller.
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

use crate::ast::{Expression, MessageSelector, MethodDefinition, MethodKind, Module};
use crate::ast_walker::walk_expression;
use crate::ffi_receiver::erlang_module_of_receiver;
use crate::semantic_analysis::ClassHierarchy;
use crate::semantic_analysis::class_hierarchy::ClassInfo;
use crate::semantic_analysis::validators::structural_validators::{
    erlang_arity, erlang_function_name,
};
use crate::source_analysis::{Diagnostic, DiagnosticCategory};

/// Why a call is surprising in the caller
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

/// One curated row: an Erlang function whose effect depends on which process
/// runs it, and its effect in the caller. `None` means the lint does not
/// fire, but the call still counts as process-sensitive
/// ([`ffi_call_is_process_sensitive`]).
#[derive(Debug)]
struct ClassMethodFfi {
    module: &'static str,
    function: &'static str,
    /// Effect when the method runs in the caller
    /// (`class-method-process-resource`).
    in_caller: Option<CallerEffect>,
}

/// Builds a [`ClassMethodFfi`] row (keeps [`CLASS_METHOD_FFI`] one line per
/// function).
const fn row(
    module: &'static str,
    function: &'static str,
    in_caller: Option<CallerEffect>,
) -> ClassMethodFfi {
    ClassMethodFfi {
        module,
        function,
        in_caller,
    }
}

const RESOURCE: Option<CallerEffect> = Some(CallerEffect::Resource);
const IDENTITY: Option<CallerEffect> = Some(CallerEffect::Identity);

/// The single curated v1 table of process-sensitive calls: those the
/// `class-method-process-resource` lint reports (an effect in the caller),
/// and those that only make an explicit class-side `new` keep the class
/// process ([`ffi_call_is_process_sensitive`], BT-4464). Modelled on the
/// curated `ffi_exports` table: a spec-derived attribute is a possible
/// follow-up. See the module docs for what is deliberately left out.
const CLASS_METHOD_FFI: &[ClassMethodFfi] = &[
    row("erlang", "self", IDENTITY),
    row("erlang", "send", None),
    row("erlang", "get", IDENTITY),
    row("erlang", "put", IDENTITY),
    row("erlang", "erase", IDENTITY),
    row("erlang", "process_flag", IDENTITY),
    row("erlang", "link", RESOURCE),
    row("erlang", "unlink", IDENTITY),
    row("erlang", "monitor", RESOURCE),
    row("erlang", "demonitor", IDENTITY),
    row("erlang", "spawn_link", RESOURCE),
    row("erlang", "open_port", RESOURCE),
    row("erlang", "register", RESOURCE),
    row("erlang", "send_after", RESOURCE),
    row("erlang", "start_timer", RESOURCE),
    row("ets", "new", RESOURCE),
    row("timer", "apply_interval", RESOURCE),
    row("timer", "apply_repeatedly", RESOURCE),
    row("timer", "send_after", RESOURCE),
    row("timer", "send_interval", RESOURCE),
    row("gen_tcp", "accept", None),
    row("gen_tcp", "connect", None),
    row("gen_tcp", "listen", None),
    row("gen_tcp", "recv", None),
    row("gen_tcp", "send", None),
    row("gen_tcp", "close", None),
    row("gen_tcp", "shutdown", None),
    row("gen_tcp", "controlling_process", None),
    row("gen_udp", "open", None),
    row("gen_udp", "recv", None),
    row("gen_udp", "send", None),
    row("gen_udp", "close", None),
    row("gen_udp", "controlling_process", None),
    row("ssl", "connect", None),
    row("ssl", "handshake", None),
    row("ssl", "recv", None),
    row("ssl", "send", None),
    row("ssl", "close", None),
    row("ssl", "shutdown", None),
    row("ssl", "controlling_process", None),
    row("file", "sendfile", None),
];

fn table_row(module: &str, function: &str) -> Option<&'static ClassMethodFfi> {
    CLASS_METHOD_FFI
        .iter()
        .find(|r| r.module == module && r.function == function)
}

/// The row matching the FFI call `(Erlang <module>) <selector>`, if any.
fn row_for_send(
    receiver: &Expression,
    selector: &MessageSelector,
) -> Option<&'static ClassMethodFfi> {
    let module = erlang_module_of_receiver(receiver)?;
    let function = erlang_function_name(selector)?;
    table_row(module, &function)
}

/// Whether `(Erlang <module>) <selector>` is any row of the curated
/// [`CLASS_METHOD_FFI`] table: a call whose effect depends on which process
/// runs it (a resource or socket it owns, its identity, its mailbox), in
/// either direction. BT-4464: an explicit class-side `new` making such a call
/// keeps the class process (`ClassHierarchy::explicit_class_new_builds_in_caller`
/// fails closed on every row), since `new` used to be guaranteed to run there,
/// the lint does not inspect process-bound selectors, and moving it would hand
/// what it creates to whichever caller ran first. Calls outside the table
/// (`file open:`, a user `start_link`) are not recognised, as for the lint.
pub(crate) fn ffi_call_is_process_sensitive(module: &str, selector: &MessageSelector) -> bool {
    erlang_function_name(selector).is_some_and(|function| table_row(module, &function).is_some())
}

/// Runs the `class-method-process-resource` lint over every class-side method
/// of `module`
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
    // `Foo` too.
    for standalone in &module.method_definitions {
        if standalone.is_class_method && standalone.method.kind == MethodKind::Primary {
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
    if !hierarchy.has_class(class_name)
        || !hierarchy.runs_class_method_in_caller(class_name, &selector)
    {
        return;
    }

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
            let Some(entry) = row_for_send(receiver, ffi_selector) else {
                return;
            };
            let Some(effect) = entry.in_caller else {
                return;
            };
            let site = Site {
                class_name,
                selector: &selector,
                call: format!(
                    "`{}:{}/{}`",
                    entry.module,
                    entry.function,
                    erlang_arity(ffi_selector, arguments.len())
                ),
                expr,
            };
            diagnostics.push(site.caller(effect));
        });
    }
}

/// A matched FFI call inside a class method: what the message names.
struct Site<'a> {
    class_name: &'a str,
    selector: &'a str,
    /// The call, rendered as `` `mod:fn/arity` ``.
    call: String,
    expr: &'a Expression,
}

impl Site<'_> {
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
            "move ownership to an Actor (create the resource in its `initialize`); class methods \
             of `{class_name}` run in whichever process calls them"
        ))
        .with_category(DiagnosticCategory::ClassMethodProcessResource)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compilation::diagnostics_policy::apply_expect_directives;
    use crate::semantic_analysis::analyse;

    fn module_of(src: &str) -> Module {
        crate::test_helpers::test_support::parse_ok(src)
    }

    fn of_category(src: &str, category: DiagnosticCategory) -> Vec<Diagnostic> {
        analyse(&module_of(src))
            .diagnostics
            .into_iter()
            .filter(|d| d.category == Some(category))
            .collect()
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
    }

    const IN_CALLER: [Probe; 2] = [Probe::SealedStateless, Probe::OpenStateless];

    /// A one-method class making `entry`'s call (every argument `x`) from
    /// `class probe: x`.
    fn probe_row(entry: &ClassMethodFfi, probe: Probe) -> String {
        let call = format!("(Erlang {}) {}: x", entry.module, entry.function);
        probe_class_calling(&call, probe)
    }

    /// A one-method class making `call` from `class probe: x`.
    fn probe_class_calling(call: &str, probe: Probe) -> String {
        match probe {
            Probe::SealedStateless => {
                format!("sealed Object subclass: Probe\n  class sealed probe: x => {call}\n")
            }
            Probe::OpenStateless => format!("Object subclass: Probe\n  class probe: x => {call}\n"),
        }
    }

    // ── table: one positive and one negative per row ─────────────────────

    #[test]
    fn rows_without_a_caller_effect_never_fire_class_method_process_resource() {
        for entry in CLASS_METHOD_FFI.iter().filter(|r| r.in_caller.is_none()) {
            for probe in IN_CALLER {
                let silent = in_caller(&probe_row(entry, probe));
                assert!(silent.is_empty(), "{entry:?} {probe:?}: {silent:?}");
            }
        }
    }

    #[test]
    fn rows_with_a_caller_effect_fire_class_method_process_resource() {
        for entry in CLASS_METHOD_FFI.iter().filter(|r| r.in_caller.is_some()) {
            for probe in IN_CALLER {
                let found = in_caller(&probe_row(entry, probe));
                assert_eq!(found.len(), 1, "{entry:?} {probe:?}: {found:?}");
            }
        }
    }

    #[test]
    fn table_has_no_duplicate_rows() {
        for (i, a) in CLASS_METHOD_FFI.iter().enumerate() {
            for b in &CLASS_METHOD_FFI[i + 1..] {
                assert!(
                    (a.module, a.function) != (b.module, b.function),
                    "duplicate row {a:?}"
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
        assert!(!h.contains("classState:"), "{h}");
    }

    // ── Ets constructors (BT-4587) ───────────────────────────────────────

    #[test]
    fn ets_constructor_in_a_class_method_is_silent() {
        // In the caller the `Ets` constructors make a table the caller owns,
        // which is what they are for.
        let src = "Object subclass: Secrets
  class table => Ets private: #set
  class shared => Ets newOrExisting: #shared type: #set
";
        assert!(in_caller(src).is_empty());
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
        }
    }

    #[test]
    fn instance_side_methods_are_silent() {
        let src = "sealed Object subclass: Inst\n  me => Erlang erlang self\n";
        assert!(in_caller(src).is_empty());
        let open = "Object subclass: Inst2\n  me => Erlang erlang self\n";
        assert!(in_caller(open).is_empty());
    }

    // ── where the method runs ───────────────────────────────────────────

    #[test]
    fn open_stateless_class_runs_in_the_caller_whatever_is_sealed() {
        // ADR 0142 §1 (BT-4301): sealing plays no part.
        for src in [
            "Object subclass: OpenC\n  class sealed me => Erlang erlang self\n",
            "sealed Object subclass: SealedC\n  class me2 => Erlang erlang self\n",
            "Object subclass: OpenD\n  class me3 => Erlang erlang self\n",
        ] {
            assert_eq!(in_caller(src).len(), 1, "{src}");
        }
    }

    #[test]
    fn unresolved_superclass_chain_still_runs_in_the_caller() {
        // ADR 0153: with no class variables, the chain plays no part in where
        // a class method runs, so an unknown superclass is no longer a doubt.
        let src = "Missing subclass: Orphan\n  class me => Erlang erlang self\n";
        assert_eq!(in_caller(src).len(), 1);
    }

    #[test]
    fn former_reflection_selectors_run_in_the_caller() {
        // ADR 0153: `hasField:`/`fieldAt:`/`fieldAt:put:`/`clearField:` are no
        // longer class-variable reflection, so a class method of that name is
        // an ordinary in-caller class method.
        for src in [
            "Object subclass: Probe\n  class hasField: x => Erlang erlang self\n",
            "Object subclass: Reg\n  class fieldAt: k => Erlang erlang self\n",
            "Object subclass: Reg\n  class fieldAt: k put: v => Erlang erlang self\n",
        ] {
            assert_eq!(in_caller(src).len(), 1, "{src}");
        }
    }

    #[test]
    fn process_bound_selectors_are_not_inspected() {
        let src = "Object subclass: Bound\n  class spawn => Erlang erlang self\n";
        assert!(in_caller(src).is_empty());
    }

    // ── category / policy / @expect ──────────────────────────────────────

    #[test]
    fn expect_category_names_round_trip() {
        let name = "class_method_process_resource";
        let cat = crate::ast::ExpectCategory::ClassMethodProcessResource;
        assert_eq!(crate::ast::ExpectCategory::from_name(name), Some(cat));
        assert_eq!(cat.as_str(), name);
        assert!(crate::ast::ExpectCategory::valid_names().contains(&name));
        // ADR 0153 removed `class-process-ffi` with the class process.
        assert_eq!(
            crate::ast::ExpectCategory::from_name("class_process_ffi"),
            None
        );
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
    fn expect_class_method_process_resource_suppresses_the_warning() {
        let src = "sealed Object subclass: SealedC
  class sealed me =>
    @expect class_method_process_resource
    Erlang erlang self
";
        assert_suppressed(src, DiagnosticCategory::ClassMethodProcessResource);
    }
}
