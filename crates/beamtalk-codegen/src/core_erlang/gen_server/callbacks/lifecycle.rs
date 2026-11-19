// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Actor `gen_server` callbacks: `format_status/1`, `code_change/3` and `terminate/2` (split from `callbacks`, BT-5041).
//!
//! **DDD Context:** Compilation — Code Generation

use crate::core_erlang::CoreErlangGenerator;

use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::form::{self, Layout};
use beamtalk_cerl_doc::{Document, INDENT, leaf, line, nest};
use beamtalk_core::ast::Module;

use super::bind;

impl CoreErlangGenerator {
    /// BT-4567: generates `format_status/1` and `'__beamtalk_secrets'/0`.
    ///
    /// `'__beamtalk_secrets'/0` answers `{Slots, Selectors}`: the class's own
    /// `class secretState` / `class secretSelectors` literals (read at compile
    /// time by `beamtalk_core::semantic_analysis::actor_secrets`), appended to
    /// its superclass module's when the superclass is itself a compiled
    /// actor. `format_status/1` hands the `gen_server` status to
    /// `beamtalk_actor_redaction:format_status/2`, which redacts from that
    /// compiled metadata, so no user code runs on the status or crash path.
    ///
    /// ```erlang
    /// 'format_status'/1 = fun (Status) ->
    ///     call 'beamtalk_actor_redaction':'format_status'('bt@store', Status)
    ///
    /// '__beamtalk_secrets'/0 = fun () ->
    ///     let <ParentSecrets> = call 'bt@base':'__beamtalk_secrets'() in
    ///     call 'beamtalk_actor_redaction':'merge_secrets'(ParentSecrets, ['token'], ['login:'])
    /// ```
    pub(in crate::core_erlang) fn generate_format_status(
        &self,
        module: &Module,
    ) -> Document<'static> {
        use beamtalk_core::semantic_analysis::actor_secrets::{SecretList, declared_secrets};

        let current_class = self.current_class(module);
        let secrets = current_class
            .map(|c| declared_secrets(module, c))
            .unwrap_or_default();
        let atom_list = |list: &SecretList| -> Document<'static> {
            let atoms = list
                .names()
                .iter()
                .map(|n| leaf::atom(n.name.to_string()))
                .collect();
            docvec!["[", Self::join_docs_with_commas(atoms), "]"]
        };
        let own_slots = atom_list(&secrets.state);
        let own_selectors = atom_list(&secrets.selectors);

        let secrets_body = match current_class.filter(|c| Self::has_parent_actor_module(c)) {
            Some(class) => form::let_values(
                [leaf::var("ParentSecrets")],
                form::call(
                    self.compiled_module_name(class.superclass_name()),
                    "__beamtalk_secrets",
                    [],
                ),
                form::call(
                    "beamtalk_actor_redaction",
                    "merge_secrets",
                    [leaf::var("ParentSecrets"), own_slots, own_selectors],
                ),
                Layout::Block,
            ),
            None => form::tuple([own_slots, own_selectors]),
        };

        docvec![
            form::fun_def(
                "format_status",
                [leaf::var("Status")],
                form::call(
                    "beamtalk_actor_redaction",
                    "format_status",
                    [
                        leaf::atom(self.module_name.to_string()),
                        leaf::var("Status"),
                    ],
                ),
                Layout::Block,
            ),
            "\n\n",
            form::fun_def("__beamtalk_secrets", [], secrets_body, Layout::Block),
            "\n",
        ]
    }

    /// Generates the `code_change/3` callback for hot code reload.
    ///
    /// Delegates to `beamtalk_hot_reload:code_change/3` for state migration.
    /// When Extra contains `{NewInstanceVars, Module}`, the hot reload service
    /// migrates fields: adds new fields with defaults, preserves existing values,
    /// and drops removed fields.
    #[allow(clippy::unused_self)] // method on impl for API consistency
    pub(in crate::core_erlang) fn generate_code_change(&self) -> Document<'static> {
        let doc = docvec![
            form::fun_def(
                "code_change",
                [leaf::var("OldVsn"), leaf::var("State"), leaf::var("Extra"),],
                form::call(
                    "beamtalk_hot_reload",
                    "code_change",
                    [leaf::var("OldVsn"), leaf::var("State"), leaf::var("Extra"),],
                ),
                Layout::Block,
            ),
            "\n",
            "\n",
        ];
        doc
    }

    /// Generates the `terminate/2` callback for `gen_server` shutdown.
    ///
    /// This calls the `terminate` method if defined.
    /// Instance tracking cleanup is automatic via process monitor.
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// 'terminate'/2 = fun (Reason, State) ->
    ///     %% Call terminate: method if defined — exceptions must not prevent shutdown
    ///     let Self = call 'beamtalk_actor':'make_self'(State) in
    ///     let _TermDisp = try call 'module':'dispatch'('terminate:', [Reason], Self, State)
    ///         of _TermOk -> 'ok'
    ///         catch <_TermT, _TermE, _TermS> -> 'ok'
    ///     in 'ok'
    /// ```
    pub(in crate::core_erlang) fn generate_terminate(
        &mut self,
        module: &Module,
    ) -> Document<'static> {
        let module_name = self.module_name.clone();

        // Use the clean Beamtalk class name for lifecycle telemetry
        // metadata, matching how dispatch traces report class names.
        let current_class = self.current_class(module);
        let class_name = current_class.map_or_else(|| module_name.clone(), |c| c.name.name.clone());

        let doc = docvec![
            form::fun_def(
                "terminate",
                [leaf::var("Reason"), leaf::var("State")],
                // Emit lifecycle stop telemetry from compiled terminate
                Self::lifecycle_call_let(
                    "lifecycle_stopped",
                    "_TelStop",
                    &class_name,
                    vec![leaf::var("Reason")],
                    docvec![
                        "%% Call terminate: method if defined — exceptions must not prevent shutdown",
                        line(),
                        bind(
                            leaf::var("Self"),
                            form::call("beamtalk_actor", "make_self", [leaf::var("State")]),
                            // Not `form::try_of_catch`: `of _TermOk` is the bare
                            // spelling (not `of <_TermOk>`), the clauses are
                            // nested under `try` rather than at its level, and
                            // `in` sits on its own line.
                            docvec![
                                "let _TermDisp = try ",
                                form::call(
                                    module_name.to_string(),
                                    "dispatch",
                                    [
                                        leaf::atom("terminate:"),
                                        Document::Str("[Reason]"),
                                        leaf::var("Self"),
                                        leaf::var("State"),
                                    ],
                                ),
                                nest(
                                    INDENT,
                                    docvec![
                                        line(),
                                        "of _TermOk -> 'ok'",
                                        line(),
                                        "catch ",
                                        form::values([
                                            leaf::var("_TermT"),
                                            leaf::var("_TermE"),
                                            leaf::var("_TermS"),
                                        ]),
                                        " -> 'ok'",
                                    ]
                                ),
                                line(),
                                "in 'ok'",
                            ],
                        ),
                    ],
                ),
                Layout::Block,
            ),
            "\n",
            "\n",
        ];
        doc
    }
}
