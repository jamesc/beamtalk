// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! OTP `gen_server` callback code generation.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Generates the standard OTP callbacks: `init/1`, `handle_cast/2`,
//! `handle_call/3`, `code_change/3`, and `terminate/2`.

use super::super::document::{Document, INDENT, line, nest};
use super::super::{CoreErlangGenerator, Result};
use crate::ast::Module;
use crate::docvec;

impl CoreErlangGenerator {
    /// Generates the `init/1` callback for `gen_server`.
    ///
    /// For classes with non-Actor superclasses, the init function:
    /// 1. Calls parent's `init(InitArgs)` to get inherited state
    /// 2. Creates a map with this class's metadata and fields
    /// 3. Merges: parent defaults → child defaults → user `InitArgs` (user wins)
    /// 4. Returns `{ok, FinalState}` or propagates parent init errors
    ///
    /// For base classes (extending Actor), it generates a simple init:
    /// 1. Creates a default state map with `$beamtalk_class`, `__methods__`, and field values
    /// 2. Merges `InitArgs` into the default state (`InitArgs` values override defaults)
    /// 3. Returns `{ok, FinalState}`
    ///
    /// # Generated Code (with inheritance)
    ///
    /// ```erlang
    /// 'init'/1 = fun (InitArgs) ->
    ///     case call 'counter':'init'(InitArgs) of
    ///         <{'ok', ParentState}> when 'true' ->
    ///             let ChildFields = ~{
    ///                 '$beamtalk_class' => 'LoggingCounter',
    ///                 '__methods__' => call 'logging_counter':'method_table'(),
    ///                 'logCount' => 0
    ///             }~
    ///             in let MergedState = call 'maps':'merge'(ParentState, ChildFields)
    ///             in let FinalState = call 'maps':'merge'(MergedState, InitArgs)
    ///             in {'ok', FinalState}
    ///         <{'error', Reason}> when 'true' ->
    ///             %% Propagate parent init error
    ///             {'error', Reason}
    ///     end
    /// ```
    ///
    /// # Generated Code (base class)
    ///
    /// ```erlang
    /// 'init'/1 = fun (InitArgs) ->
    ///     let DefaultState = ~{
    ///         '$beamtalk_class' => 'Counter',
    ///         '__methods__' => call 'counter':'method_table'(),
    ///         'value' => 0
    ///     }~
    ///     in let FinalState = call 'maps':'merge'(DefaultState, InitArgs)
    ///        in {'ok', FinalState}
    /// ```
    #[allow(clippy::too_many_lines)] // inheritance-aware init with parent state merge
    pub(in crate::codegen::core_erlang) fn generate_init_function(
        &mut self,
        module: &Module,
    ) -> Result<Document<'static>> {
        // Find the current class to check for superclass
        // NOTE: This requires the .bt file to have an explicit class definition
        // like "Counter subclass: LoggingCounter" (see tests/fixtures/logging_counter.bt).
        // Module-level expressions without a class definition take the base class path below.
        let current_class = module.classes.iter().find(|c| {
            use super::super::util::module_matches_class;
            module_matches_class(&self.module_name, &c.name.name)
        });

        // Check if we have a superclass that's not Actor (base class)
        // When true, we'll call the parent's init to inherit state fields
        let has_parent_init = if let Some(class) = current_class {
            class.superclass.as_ref().is_some_and(|s| {
                !s.name.eq_ignore_ascii_case("Actor") && !s.name.eq_ignore_ascii_case("Object")
            })
        } else {
            false
        };

        let class_name = self.class_name();
        let module_name = self.module_name.clone();

        if has_parent_init {
            // Call parent's init to get inherited state, then merge with our state
            // SAFETY: has_parent_init is true only when current_class.is_some(),
            // so this expect cannot fail unless there's a logic error
            let class = current_class.expect("has_parent_init implies current_class is Some");
            let parent_module = {
                // ADR 0016 / BT-794: Use the same module naming logic as
                // superclass_module_name() which handles stdlib (bt@stdlib@*),
                // package (bt@{pkg}@*), and legacy (bt@*) modules
                self.compiled_module_name(class.superclass_name())
            };

            // Get this class's own state fields
            let own_state_fields = self.generate_own_state_fields(module)?;

            let doc = docvec![
                "'init'/1 = fun (InitArgs) ->",
                nest(
                    INDENT,
                    docvec![
                        line(),
                        "%% Call parent init to get inherited state fields",
                        line(),
                        docvec![
                            "case call '",
                            Document::String(parent_module.clone()),
                            "':'init'(InitArgs) of"
                        ],
                        nest(
                            INDENT,
                            docvec![
                                line(),
                                "<{'ok', ParentState}> when 'true' ->",
                                nest(
                                    INDENT,
                                    docvec![
                                        line(),
                                        "%% Merge parent state with this class's fields",
                                        line(),
                                        "let ChildFields = ~{",
                                        nest(
                                            INDENT,
                                            docvec![
                                                line(),
                                                docvec![
                                                    "'$beamtalk_class' => '",
                                                    Document::String(class_name.clone()),
                                                    "',"
                                                ],
                                                line(),
                                                docvec![
                                                    "'__class_mod__' => '",
                                                    Document::String(module_name.clone()),
                                                    "',"
                                                ],
                                                line(),
                                                docvec![
                                                    "'__methods__' => call '",
                                                    Document::String(module_name.clone()),
                                                    "':'method_table'()"
                                                ],
                                                Document::Vec(own_state_fields),
                                            ]
                                        ),
                                        line(),
                                        "}~",
                                        line(),
                                        "in let MergedState = call 'maps':'merge'(ParentState, ChildFields)",
                                        line(),
                                        // Merge InitArgs last so user-provided values override defaults
                                        // Order: parent defaults → child defaults → user overrides
                                        "in let FinalState = call 'maps':'merge'(MergedState, InitArgs)",
                                        line(),
                                        "in {'ok', FinalState}",
                                    ]
                                ),
                                line(),
                                "<{'error', Reason}> when 'true' ->",
                                nest(
                                    INDENT,
                                    docvec![
                                        line(),
                                        "%% Propagate parent init error",
                                        line(),
                                        "{'error', Reason}",
                                    ]
                                ),
                            ]
                        ),
                        line(),
                        "end",
                    ]
                ),
                "\n",
                "\n",
            ];
            Ok(doc)
        } else {
            // No parent, or parent is Actor base class - generate normal init
            let initial_state_fields = self.generate_initial_state_fields(module)?;

            let doc = docvec![
                "'init'/1 = fun (InitArgs) ->",
                nest(
                    INDENT,
                    docvec![
                        line(),
                        "let DefaultState = ~{",
                        nest(
                            INDENT,
                            docvec![
                                line(),
                                docvec![
                                    "'$beamtalk_class' => '",
                                    Document::String(class_name.clone()),
                                    "',"
                                ],
                                line(),
                                docvec![
                                    "'__class_mod__' => '",
                                    Document::String(module_name.clone()),
                                    "',"
                                ],
                                line(),
                                docvec![
                                    "'__methods__' => call '",
                                    Document::String(module_name.clone()),
                                    "':'method_table'()"
                                ],
                                Document::Vec(initial_state_fields),
                            ]
                        ),
                        line(),
                        "}~",
                        line(),
                        // Merge InitArgs into DefaultState - InitArgs values override defaults
                        "in let FinalState = call 'maps':'merge'(DefaultState, InitArgs)",
                        line(),
                        "in {'ok', FinalState}",
                    ]
                ),
                "\n",
                "\n",
            ];
            Ok(doc)
        }
    }

    /// Generates the `handle_cast/2` callback for async message sends.
    ///
    /// Per BT-29 design doc, uses `safe_dispatch/3` for error isolation and
    /// sends `{resolve, Result}` or `{reject, Error}` to the `FuturePid`.
    #[allow(clippy::unnecessary_wraps)] // uniform Result<Document> codegen interface
    pub(in crate::codegen::core_erlang) fn generate_handle_cast(
        &mut self,
    ) -> Result<Document<'static>> {
        let module_name = self.module_name.clone();
        let doc = docvec![
            "'handle_cast'/2 = fun (Msg, State) ->",
            nest(
                INDENT,
                docvec![
                    line(),
                    "case Msg of",
                    nest(
                        INDENT,
                        docvec![
                            line(),
                            "<{Selector, Args, FuturePid}> when 'true' ->",
                            nest(
                                INDENT,
                                docvec![
                                    line(),
                                    // Store FuturePid so @primitive dispatch/3 can access it
                                    // (e.g. subscribe/unsubscribe need to identify the caller)
                                    "let _SavedFuture = call 'erlang':'put'('$bt_future_pid', FuturePid)",
                                    line(),
                                    // Use safe_dispatch for error isolation per BT-29
                                    docvec![
                                        "in case call '",
                                        Document::String(module_name.clone()),
                                        "':'safe_dispatch'(Selector, Args, State) of"
                                    ],
                                    nest(
                                        INDENT,
                                        docvec![
                                            // Success case: resolve the future
                                            line(),
                                            "<{'reply', Result, NewState}> when 'true' ->",
                                            nest(
                                                INDENT,
                                                docvec![
                                                    line(),
                                                    // Send {resolve, Result} to match beamtalk_future protocol
                                                    "let _Ignored = call 'erlang':'!'(FuturePid, {'resolve', Result})",
                                                    line(),
                                                    "in {'noreply', NewState}",
                                                ]
                                            ),
                                            // Error case: reject the future (error isolation)
                                            line(),
                                            "<{'error', Error, NewState}> when 'true' ->",
                                            nest(
                                                INDENT,
                                                docvec![
                                                    line(),
                                                    // Send {reject, Error} to match beamtalk_future protocol
                                                    "let _Ignored = call 'erlang':'!'(FuturePid, {'reject', Error})",
                                                    line(),
                                                    "in {'noreply', NewState}",
                                                ]
                                            ),
                                        ]
                                    ),
                                    line(),
                                    "end",
                                ]
                            ),
                        ]
                    ),
                    line(),
                    "end",
                ]
            ),
            "\n",
            "\n",
        ];
        Ok(doc)
    }

    /// Generates the `handle_call/3` callback for sync message sends.
    ///
    /// Per BT-29 design doc, uses `safe_dispatch/3` for error isolation and
    /// returns `{ok, Result}` or `{error, Error}` tuples.
    #[allow(clippy::unnecessary_wraps)] // uniform Result<Document> codegen interface
    pub(in crate::codegen::core_erlang) fn generate_handle_call(
        &mut self,
    ) -> Result<Document<'static>> {
        let module_name = self.module_name.clone();
        let doc = docvec![
            "'handle_call'/3 = fun (Msg, _From, State) ->",
            nest(
                INDENT,
                docvec![
                    line(),
                    "case Msg of",
                    nest(
                        INDENT,
                        docvec![
                            line(),
                            "<{Selector, Args}> when 'true' ->",
                            nest(
                                INDENT,
                                docvec![
                                    line(),
                                    // Use safe_dispatch for error isolation per BT-29
                                    docvec![
                                        "case call '",
                                        Document::String(module_name.clone()),
                                        "':'safe_dispatch'(Selector, Args, State) of"
                                    ],
                                    nest(
                                        INDENT,
                                        docvec![
                                            // Success case: return {ok, Result}
                                            line(),
                                            "<{'reply', Result, NewState}> when 'true' ->",
                                            nest(
                                                INDENT,
                                                docvec![
                                                    line(),
                                                    "{'reply', {'ok', Result}, NewState}",
                                                ]
                                            ),
                                            // Error case: return {error, Error}
                                            line(),
                                            "<{'error', Error, NewState}> when 'true' ->",
                                            nest(
                                                INDENT,
                                                docvec![
                                                    line(),
                                                    "{'reply', {'error', Error}, NewState}",
                                                ]
                                            ),
                                        ]
                                    ),
                                    line(),
                                    "end",
                                ]
                            ),
                        ]
                    ),
                    line(),
                    "end",
                ]
            ),
            "\n",
            "\n",
        ];
        Ok(doc)
    }

    /// Generates the `code_change/3` callback for hot code reload.
    ///
    /// Delegates to `beamtalk_hot_reload:code_change/3` for state migration.
    /// When Extra contains `{NewInstanceVars, Module}`, the hot reload service
    /// migrates fields: adds new fields with defaults, preserves existing values,
    /// and drops removed fields.
    #[allow(clippy::unused_self)] // method on impl for API consistency
    #[allow(clippy::unnecessary_wraps)] // uniform Result<Document> codegen interface
    pub(in crate::codegen::core_erlang) fn generate_code_change(
        &self,
    ) -> Result<Document<'static>> {
        let doc = docvec![
            "'code_change'/3 = fun (OldVsn, State, Extra) ->",
            nest(
                INDENT,
                docvec![
                    line(),
                    "call 'beamtalk_hot_reload':'code_change'(OldVsn, State, Extra)",
                ]
            ),
            "\n",
            "\n",
        ];
        Ok(doc)
    }

    /// Generates the `terminate/2` callback for `gen_server` shutdown.
    ///
    /// Per BT-29 design doc, this calls the `terminate` method if defined.
    /// Instance tracking cleanup (BT-96) is automatic via process monitor.
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// 'terminate'/2 = fun (Reason, State) ->
    ///     %% Call terminate method if defined
    ///     let Self = call 'beamtalk_actor':'make_self'(State) in
    ///     case call 'module':'dispatch'('terminate', [Reason], Self, State) of
    ///         <{'reply', _Result, _State}> when 'true' -> 'ok'
    ///         <_Other> when 'true' -> 'ok'
    ///     end
    /// ```
    #[allow(clippy::unnecessary_wraps)] // uniform Result<Document> codegen interface
    pub(in crate::codegen::core_erlang) fn generate_terminate(
        &mut self,
        _module: &Module,
    ) -> Result<Document<'static>> {
        let module_name = self.module_name.clone();
        let doc = docvec![
            "'terminate'/2 = fun (Reason, State) ->",
            nest(
                INDENT,
                docvec![
                    line(),
                    "%% Call terminate method if defined",
                    line(),
                    "let Self = call 'beamtalk_actor':'make_self'(State) in",
                    line(),
                    docvec![
                        "case call '",
                        Document::String(module_name.clone()),
                        "':'dispatch'('terminate', [Reason], Self, State) of"
                    ],
                    nest(
                        INDENT,
                        docvec![
                            line(),
                            // Core Erlang doesn't allow duplicate _ patterns, use unique ignored vars
                            "<{'reply', _TermResult, _TermState}> when 'true' -> 'ok'",
                            line(),
                            // dispatch returns 3-tuple {'error', Error, State}, not 2-tuple
                            "<{'error', _TermError, _TermState2}> when 'true' -> 'ok'",
                            line(),
                            "<_TermOther> when 'true' -> 'ok'",
                        ]
                    ),
                    line(),
                    "end",
                ]
            ),
            "\n",
            "\n",
        ];
        Ok(doc)
    }
}
