// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! OTP `gen_server` callback code generation.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Generates the standard OTP callbacks: `init/1`, `handle_continue/2`,
//! `handle_cast/2`, `handle_call/3`, `handle_info/2`, `code_change/3`,
//! and `terminate/2`.

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
    /// 1. Creates a default state map with `__class_mod__` and field values
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
    ///                 '__class_mod__' => 'logging_counter',
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
    ///         '__class_mod__' => 'counter',
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

        // BT-1417: Check if the class defines an initialize method.
        // If so, dispatch it at the end of init/1 — but only when not called
        // as a parent state-building helper. The __skip_initialize__ flag in
        // InitArgs suppresses dispatch when a child's init calls us as a helper.
        let has_initialize = current_class.is_some_and(|c| {
            self.semantic_facts
                .class_facts(&c.name.name)
                .is_some_and(|cf| cf.has_instance_method("initialize"))
        });

        let module_name = self.module_name.clone();

        // BT-1417: Generate the init return — either plain {ok, State} or
        // dispatch initialize first, then return {ok, NewState} / {stop, Error}.
        // When has_initialize is true, we wrap the dispatch in a guard that
        // checks __skip_initialize__ in InitArgs, so parent helpers skip it.
        let init_return = if has_initialize {
            Self::init_initialize_guarded_doc()
        } else {
            Document::Str("in {'ok', FinalState}")
        };

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
                        // BT-1417: Pass __skip_initialize__ to parent so it
                        // doesn't dispatch initialize — only the leaf should.
                        "let _ParentArgs = call 'maps':'put'('__skip_initialize__', 'true', InitArgs) in",
                        line(),
                        docvec![
                            "case call '",
                            Document::String(parent_module.clone()),
                            "':'init'(_ParentArgs) of"
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
                                                    "'__class_mod__' => '",
                                                    Document::String(module_name.clone()),
                                                    "'"
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
                                        init_return.clone(),
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
                                    "'__class_mod__' => '",
                                    Document::String(module_name.clone()),
                                    "'"
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
                        init_return,
                    ]
                ),
                "\n",
                "\n",
            ];
            Ok(doc)
        }
    }

    /// BT-1417/BT-1541: Generate the guarded initialize dispatch block for init/1.
    ///
    /// Checks `__skip_initialize__` in `InitArgs` — when a child's init calls
    /// us as a parent state helper, it sets this flag to prevent double dispatch.
    /// When called as the outermost init (by OTP), the flag is absent and
    /// initialize is deferred to `handle_continue`.
    ///
    /// BT-1541: Uses OTP's `handle_continue` pattern instead of dispatching
    /// initialize inline. This ensures the `gen_server` message loop is running
    /// when initialize executes, so self-sends don't deadlock. OTP guarantees
    /// no messages arrive before `handle_continue` runs.
    fn init_initialize_guarded_doc() -> Document<'static> {
        docvec![
            "%% BT-1417/BT-1541: Defer initialize to handle_continue unless called as a parent helper",
            line(),
            "in case call 'maps':'get'('__skip_initialize__', InitArgs, 'false') of",
            nest(
                INDENT,
                docvec![
                    line(),
                    "<'true'> when 'true' ->",
                    nest(
                        INDENT,
                        docvec![
                            line(),
                            // Strip the flag from state before returning
                            "let CleanState = call 'maps':'remove'('__skip_initialize__', FinalState) in",
                            line(),
                            "{'ok', CleanState}",
                        ]
                    ),
                    line(),
                    "<'false'> when 'true' ->",
                    nest(
                        INDENT,
                        docvec![
                            line(),
                            // Strip the flag from state before returning
                            "let CleanState1 = call 'maps':'remove'('__skip_initialize__', FinalState) in",
                            line(),
                            // BT-1541: Return {ok, State, {continue, initialize}} to defer
                            // initialize dispatch to handle_continue where the loop is running
                            "{'ok', CleanState1, {'continue', 'initialize'}}",
                        ]
                    ),
                ]
            ),
            line(),
            "end",
        ]
    }

    /// Generates the `handle_continue/2` callback (BT-1541).
    ///
    /// Dispatches `initialize` via `safe_dispatch` when the continuation token
    /// `{continue, initialize}` arrives. This runs after `init/1` returns, so
    /// the `gen_server` message loop is active and self-sends work without deadlock.
    ///
    /// OTP guarantees no other messages are processed before `handle_continue`.
    ///
    /// # Generated Code
    ///
    /// ```erlang
    /// 'handle_continue'/2 = fun (Continue, State) ->
    ///     case Continue of
    ///         <'initialize'> when 'true' ->
    ///             case call 'module':'safe_dispatch'('initialize', [], State) of
    ///                 <{'reply', _InitResult, InitNewState}> when 'true' ->
    ///                     {'noreply', InitNewState}
    ///                 <{'error', InitError, _InitErrState}> when 'true' ->
    ///                     {'stop', InitError, State}
    ///             end
    ///         <_> when 'true' -> {'noreply', State}
    ///     end
    /// ```
    #[allow(clippy::unnecessary_wraps)] // uniform Result<Document> codegen interface
    pub(in crate::codegen::core_erlang) fn generate_handle_continue(
        &self,
    ) -> Result<Document<'static>> {
        let module_name = self.module_name.clone();
        let doc = docvec![
            "'handle_continue'/2 = fun (Continue, State) ->",
            nest(
                INDENT,
                docvec![
                    line(),
                    "case Continue of",
                    nest(
                        INDENT,
                        docvec![
                            line(),
                            "<'initialize'> when 'true' ->",
                            nest(
                                INDENT,
                                docvec![
                                    line(),
                                    docvec![
                                        "case call '",
                                        Document::String(module_name),
                                        "':'safe_dispatch'('initialize', [], State) of",
                                    ],
                                    nest(
                                        INDENT,
                                        docvec![
                                            line(),
                                            "<{'reply', _InitResult, InitNewState}> when 'true' ->",
                                            nest(
                                                INDENT,
                                                docvec![line(), "{'noreply', InitNewState}"]
                                            ),
                                            line(),
                                            "<{'error', InitError, _InitErrState}> when 'true' ->",
                                            nest(
                                                INDENT,
                                                docvec![line(), "{'stop', InitError, State}"]
                                            ),
                                        ]
                                    ),
                                    line(),
                                    "end",
                                ]
                            ),
                            line(),
                            "<_> when 'true' -> {'noreply', State}",
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

    /// Generates the `handle_cast/2` callback for async message sends.
    ///
    /// Handles the fire-and-forget cast format: `{cast, Selector, Args}` — sent by
    /// `beamtalk_actor:cast_send/3`. Dispatches the message and updates state;
    /// errors are logged via `logger:warning` and discarded (BT-943).
    // BT-920: helper — generates the inner `case safe_dispatch ... end` for fire-and-forget casts.
    fn cast_dispatch_case(module_name: &str) -> Document<'static> {
        docvec![
            line(),
            // Use safe_dispatch for error isolation; discard result on error
            docvec![
                "case call '",
                Document::String(module_name.to_owned()),
                "':'safe_dispatch'(CastSelector, CastArgs, State) of"
            ],
            nest(
                INDENT,
                docvec![
                    line(),
                    "<{'reply', _CastResult, CastNewState}> when 'true' ->",
                    nest(INDENT, docvec![line(), "{'noreply', CastNewState}"]),
                    line(),
                    // BT-943: Log error but don't crash — caller expects no reply
                    "<{'error', CastError, _CastState}> when 'true' ->",
                    nest(
                        INDENT,
                        docvec![
                            line(),
                            "let _ = call 'logger':'warning'(~{'selector' => CastSelector, 'reason' => CastError}~)",
                            line(),
                            "in {'noreply', State}",
                        ]
                    ),
                ]
            ),
            line(),
            "end",
        ]
    }

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
                            // BT-920: Fire-and-forget cast {cast, Selector, Args}
                            line(),
                            "<{'cast', CastSelector, CastArgs}> when 'true' ->",
                            nest(INDENT, Self::cast_dispatch_case(&module_name)),
                            line(),
                            "<_> when 'true' -> {'noreply', State}",
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

    /// Generates the `handle_info/2` callback (BT-936, ADR 0065 / BT-1457).
    ///
    /// For **Server subclasses**, dispatches to the user-defined `handleInfo:` method
    /// with log-and-continue error semantics: if `handleInfo:` raises an error, the
    /// server logs a warning and continues with the pre-call state.
    ///
    /// For **plain Actor subclasses**, generates the default ignore-all stub that
    /// delegates to `beamtalk_actor:handle_info/2`.
    ///
    /// # Generated Code (Server subclass)
    ///
    /// ```erlang
    /// 'handle_info'/2 = fun (Msg, State) ->
    ///     case call 'Module':'safe_dispatch'('handleInfo:', [Msg], State) of
    ///         <{'reply', _Result, NewState}> when 'true' -> {'noreply', NewState}
    ///         <{'error', Error, _ErrState}> when 'true' ->
    ///             let _Log = call 'logger':'warning'(~{'handleInfo_error' => Error}~)
    ///             in {'noreply', State}
    ///         <_Other> when 'true' -> {'noreply', State}
    ///     end
    /// ```
    ///
    /// # Generated Code (plain Actor)
    ///
    /// ```erlang
    /// 'handle_info'/2 = fun (Msg, State) ->
    ///     call 'beamtalk_actor':'handle_info'(Msg, State)
    /// ```
    #[allow(clippy::unnecessary_wraps)] // uniform Result<Document> codegen interface
    pub(in crate::codegen::core_erlang) fn generate_handle_info(
        &self,
    ) -> Result<Document<'static>> {
        if self.is_server_subclass {
            let module_name = self.module_name.clone();
            let doc = docvec![
                "'handle_info'/2 = fun (Msg, State) ->",
                nest(
                    INDENT,
                    docvec![
                        line(),
                        docvec![
                            "case call '",
                            Document::String(module_name),
                            "':'safe_dispatch'('handleInfo:', [Msg], State) of",
                        ],
                        nest(
                            INDENT,
                            docvec![
                                line(),
                                "<{'reply', _Result, NewState}> when 'true' ->",
                                nest(INDENT, docvec![line(), "{'noreply', NewState}",]),
                                line(),
                                "<{'error', Error, _ErrState}> when 'true' ->",
                                nest(
                                    INDENT,
                                    docvec![
                                        line(),
                                        "let _Log = call 'logger':'warning'(~{'handleInfo_error' => Error}~)",
                                        line(),
                                        "in {'noreply', State}",
                                    ]
                                ),
                                line(),
                                "<_Other> when 'true' -> {'noreply', State}",
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
            let doc = docvec![
                "'handle_info'/2 = fun (Msg, State) ->",
                nest(
                    INDENT,
                    docvec![line(), "call 'beamtalk_actor':'handle_info'(Msg, State)",]
                ),
                "\n",
                "\n",
            ];
            Ok(doc)
        }
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
    ///     %% Call terminate: method if defined — exceptions must not prevent shutdown
    ///     let Self = call 'beamtalk_actor':'make_self'(State) in
    ///     let _TermDisp = try call 'module':'dispatch'('terminate:', [Reason], Self, State)
    ///         of _TermOk -> 'ok'
    ///         catch <_TermT, _TermE, _TermS> -> 'ok'
    ///     in 'ok'
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
                    "%% Call terminate: method if defined — exceptions must not prevent shutdown",
                    line(),
                    "let Self = call 'beamtalk_actor':'make_self'(State) in",
                    line(),
                    docvec![
                        "let _TermDisp = try call '",
                        Document::String(module_name.clone()),
                        "':'dispatch'('terminate:', [Reason], Self, State)"
                    ],
                    nest(
                        INDENT,
                        docvec![
                            line(),
                            "of _TermOk -> 'ok'",
                            line(),
                            "catch <_TermT, _TermE, _TermS> -> 'ok'",
                        ]
                    ),
                    line(),
                    "in 'ok'",
                ]
            ),
            "\n",
            "\n",
        ];
        Ok(doc)
    }
}
