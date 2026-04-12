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
use crate::ast::{ClassDefinition, Module, TypeAnnotation};
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

        // BT-1642: Use the clean Beamtalk class name (e.g., "EventStore") for
        // lifecycle telemetry metadata instead of the compiled Erlang module name
        // (e.g., "bt@exdura@event_store"). This matches how dispatch traces
        // report class names via lookup_class/1.
        let class_name = current_class.map_or_else(|| module_name.clone(), |c| c.name.name.clone());

        // BT-1417: Generate the init return — either plain {ok, State} or
        // dispatch initialize first, then return {ok, NewState} / {stop, Error}.
        // When has_initialize is true, we wrap the dispatch in a guard that
        // checks __skip_initialize__ in InitArgs, so parent helpers skip it.
        // BT-1638: Lifecycle start telemetry is emitted here, guarded by
        // __skip_initialize__ so parent helper calls don't double-fire.
        let init_return = if has_initialize {
            Self::init_initialize_guarded_doc(&class_name)
        } else {
            Self::init_plain_return_doc(&class_name)
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
                                                    Document::String(module_name.to_string()),
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
                                    Document::String(module_name.to_string()),
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

    /// BT-1638: Generate the lifecycle start telemetry call for init/1.
    ///
    /// Emits `[beamtalk, actor, lifecycle, start]` via `beamtalk_actor:maybe_execute_telemetry/3`.
    /// Uses a `let` binding for `self()` since Core Erlang map literals cannot contain calls.
    /// BT-1642: Takes the clean Beamtalk class name (e.g., `EventStore`), not the
    /// compiled module name, so lifecycle traces match dispatch trace format.
    fn lifecycle_start_telemetry_doc(class_name: &str) -> Document<'static> {
        docvec![
            "let _TelPid = call 'erlang':'self'() in",
            line(),
            docvec![
                "let _TelStart = call 'beamtalk_actor':'maybe_execute_telemetry'(",
                "['beamtalk', 'actor', 'lifecycle', 'start'], ~{}~, ~{'pid' => _TelPid, 'class' => '",
                Document::String(class_name.to_owned()),
                "'}~) in",
            ],
        ]
    }

    /// BT-1638: Generate a plain init return with lifecycle telemetry.
    ///
    /// For classes WITHOUT `initialize`, emits lifecycle start telemetry
    /// guarded by `__skip_initialize__` so parent helper calls don't double-fire.
    /// When called as a parent helper (flag is 'true'), skips telemetry and
    /// returns `{ok, State}` directly.
    fn init_plain_return_doc(class_name: &str) -> Document<'static> {
        docvec![
            // BT-1638: Guard telemetry behind __skip_initialize__ to avoid
            // double-fire when called as parent state-building helper
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
                            Self::lifecycle_start_telemetry_doc(class_name),
                            line(),
                            "{'ok', FinalState}",
                        ]
                    ),
                ]
            ),
            line(),
            "end",
        ]
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
    ///
    /// BT-1638: Also emits lifecycle start telemetry in the non-helper branch.
    fn init_initialize_guarded_doc(class_name: &str) -> Document<'static> {
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
                            // BT-1638: Emit lifecycle start telemetry before returning
                            Self::lifecycle_start_telemetry_doc(class_name),
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

    /// BT-1949: Generates the success body for `handle_continue` after initialize.
    ///
    /// If the class has typed-no-default state fields (type annotation but no `= default`),
    /// emits a check that each such field is no longer nil. If any are still nil,
    /// raises `UninitializedStateError` with the class name, field name, and expected type.
    /// If there are no such fields, returns a plain `{'noreply', InitNewState}`.
    #[allow(clippy::too_many_lines)]
    fn generate_post_initialize_check(
        &self,
        current_class: Option<&ClassDefinition>,
        class_name: &ecow::EcoString,
    ) -> Document<'static> {
        // Collect typed-no-default fields: have type annotation, no default value,
        // and the type is NOT nilable (a union containing Nil — nil is a valid value).
        let typed_no_default: Vec<_> = current_class
            .map(|c| {
                c.state
                    .iter()
                    .filter(|s| {
                        s.type_annotation.is_some()
                            && s.default_value.is_none()
                            && !Self::is_nilable_type(s.type_annotation.as_ref())
                    })
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default();

        if typed_no_default.is_empty() {
            return docvec![line(), "{'noreply', InitNewState}"];
        }

        // Build nested checks: each field gets a case that either stops or continues.
        // We build from the inside out — the innermost is {'noreply', InitNewState}.
        let mut body: Document<'static> = docvec!["{'noreply', InitNewState}"];

        for (i, field) in typed_no_default.iter().enumerate().rev() {
            let field_name = field.name.name.to_string();
            let type_name = field
                .type_annotation
                .as_ref()
                .map_or_else(|| "Unknown".to_string(), Self::type_annotation_display);
            let idx = i.to_string();
            let check_var = format!("_ChkVal{idx}");
            let err_var0 = format!("_UErr{idx}a");
            let err_var1 = format!("_UErr{idx}b");
            let err_msg_var = format!("_UErrMsg{idx}");
            let class_name_var = format!("_UErrClass{idx}");
            let hint_msg =
                format!("{class_name} field '{field_name}' (:: {type_name}) was not initialized",);
            let hint_binary = Self::binary_string_literal(&hint_msg);

            body = docvec![
                "let ",
                Document::String(check_var.clone()),
                " = call 'maps':'get'('",
                Document::String(field_name),
                "', InitNewState) in",
                line(),
                "case ",
                Document::String(check_var),
                " of",
                nest(
                    INDENT,
                    docvec![
                        line(),
                        "<'nil'> when 'true' ->",
                        nest(
                            INDENT,
                            docvec![
                                line(),
                                "let ",
                                Document::String(err_var0.clone()),
                                " = call 'beamtalk_error':'new'('uninitialized_state_error', '",
                                Document::String(class_name.to_string()),
                                "') in",
                                line(),
                                "let ",
                                Document::String(err_var1.clone()),
                                " = call 'beamtalk_error':'with_hint'(",
                                Document::String(err_var0),
                                ", ",
                                Document::String(hint_binary),
                                ") in",
                                line(),
                                "let ",
                                Document::String(err_msg_var.clone()),
                                " = call 'beamtalk_error':'format_safe'(",
                                Document::String(err_var1.clone()),
                                ") in",
                                line(),
                                "let ",
                                Document::String(class_name_var.clone()),
                                " = call '",
                                Document::Eco(self.module_name.clone()),
                                "':'class_name'() in",
                                line(),
                                "let _ = call 'logger':'error'(",
                                Document::String(err_msg_var),
                                ", ~{'class' => ",
                                Document::String(class_name_var),
                                ", 'reason' => ",
                                Document::String(err_var1.clone()),
                                ", 'domain' => ['beamtalk'|['runtime'|[]]]}~) in",
                                line(),
                                "{'stop', ",
                                Document::String(err_var1),
                                ", InitNewState}",
                            ]
                        ),
                        line(),
                        "<_> when 'true' ->",
                        nest(INDENT, docvec![line(), body]),
                    ]
                ),
                line(),
                "end",
            ];
        }

        docvec![
            line(),
            "%% BT-1949: Verify typed-no-default fields were initialized",
            line(),
            body,
        ]
    }

    /// Returns true if the type annotation includes `Nil` (making nil a valid value).
    fn is_nilable_type(ta: Option<&TypeAnnotation>) -> bool {
        match ta {
            Some(TypeAnnotation::Simple(id)) => id.name == "Nil",
            Some(TypeAnnotation::Union { types, .. }) => {
                types.iter().any(|t| Self::is_nilable_type(Some(t)))
            }
            _ => false,
        }
    }

    /// Returns a human-readable string for a type annotation (for error messages).
    fn type_annotation_display(ta: &TypeAnnotation) -> String {
        match ta {
            TypeAnnotation::Simple(id) => id.name.to_string(),
            TypeAnnotation::Union { types, .. } => types
                .iter()
                .map(Self::type_annotation_display)
                .collect::<Vec<_>>()
                .join(" | "),
            TypeAnnotation::Singleton { name, .. } => format!("#{name}"),
            TypeAnnotation::Generic {
                base, parameters, ..
            } => {
                let params = parameters
                    .iter()
                    .map(Self::type_annotation_display)
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}({params})", base.name)
            }
            TypeAnnotation::FalseOr { inner, .. } => {
                format!("{} | False", Self::type_annotation_display(inner))
            }
            TypeAnnotation::SelfType { .. } => "Self".to_string(),
        }
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
    ///                 <{'error', {InitType, InitReason, InitStacktrace}, InitErrState}> when 'true' ->
    ///                     let _ = call 'logger':'error'(Msg, #{stacktrace => ...}) in
    ///                     {'stop', {InitType, InitReason, InitStacktrace}, InitErrState}
    ///                 <{'error', InitError, InitErrState2}> when 'true' ->
    ///                     let _ = call 'logger':'error'(Msg, #{...}) in
    ///                     {'stop', InitError, InitErrState2}
    ///             end
    ///         <_> when 'true' -> {'noreply', State}
    ///     end
    /// ```
    #[allow(clippy::unnecessary_wraps)] // uniform Result<Document> codegen interface
    pub(in crate::codegen::core_erlang) fn generate_handle_continue(
        &self,
        module: &Module,
    ) -> Result<Document<'static>> {
        let module_name = self.module_name.clone();
        let module_name_for_log = module_name.clone();

        // BT-1949: Find typed-no-default fields for post-initialize validation.
        let current_class = module.classes.iter().find(|c| {
            use super::super::util::module_matches_class;
            module_matches_class(&self.module_name, &c.name.name)
        });
        let class_name = current_class.map_or_else(|| module_name.clone(), |c| c.name.name.clone());
        let success_body = self.generate_post_initialize_check(current_class, &class_name);

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
                                    // BT-1325: Stash State for re-entrant self-sends
                                    Self::pdict_stash_preamble(),
                                    line(),
                                    docvec![
                                        "let _InitDispatchResult = call '",
                                        Document::Eco(module_name),
                                        "':'safe_dispatch'('initialize', [], State) in",
                                    ],
                                    Self::pdict_restore_epilogue(),
                                    line(),
                                    "case _InitDispatchResult of",
                                    nest(
                                        INDENT,
                                        docvec![
                                            line(),
                                            "<{'reply', _InitResult, InitNewState}> when 'true' ->",
                                            nest(INDENT, success_body,),
                                            line(),
                                            // BT-1822: Destructure error triple to capture stacktrace
                                            "<{'error', {InitType, InitReason, InitStacktrace}, InitErrState}> when 'true' ->",
                                            nest(
                                                INDENT,
                                                docvec![
                                                    line(),
                                                    "let InitErrorMsg = call 'beamtalk_error':'format_safe'({InitType, InitReason}, InitStacktrace) in",
                                                    line(),
                                                    "let InitClassName = call '",
                                                    Document::Eco(module_name_for_log.clone()),
                                                    "':'class_name'() in",
                                                    line(),
                                                    "let _ = call 'logger':'error'(InitErrorMsg, ~{'class' => InitClassName, 'reason' => {InitType, InitReason}, 'stacktrace' => InitStacktrace, 'domain' => ['beamtalk'|['runtime'|[]]]}~) in",
                                                    line(),
                                                    "{'stop', {InitType, InitReason, InitStacktrace}, InitErrState}",
                                                ]
                                            ),
                                            line(),
                                            // Fallback: plain {error, Error, State} from dispatch (DNU, #beamtalk_error{}, etc.)
                                            "<{'error', InitError, InitErrState2}> when 'true' ->",
                                            nest(
                                                INDENT,
                                                docvec![
                                                    line(),
                                                    "let InitErrorMsg2 = call 'beamtalk_error':'format_safe'(InitError) in",
                                                    line(),
                                                    "let InitClassName2 = call '",
                                                    Document::Eco(module_name_for_log),
                                                    "':'class_name'() in",
                                                    line(),
                                                    "let _ = call 'logger':'error'(InitErrorMsg2, ~{'class' => InitClassName2, 'reason' => InitError, 'domain' => ['beamtalk'|['runtime'|[]]]}~) in",
                                                    line(),
                                                    "{'stop', InitError, InitErrState2}",
                                                ]
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
    /// BT-1325: Generates the pdict stash preamble for re-entrant self-sends.
    /// Returns `let _OldState = ... in let _PutOk = ... in` — caller appends
    /// the dispatch body and must call `pdict_restore_epilogue()` after.
    fn pdict_stash_preamble() -> Document<'static> {
        docvec![
            line(),
            "let _OldState = call 'erlang':'get'('$bt_actor_state') in",
            line(),
            "let _PutOk = call 'erlang':'put'('$bt_actor_state', State) in",
        ]
    }

    /// BT-1325: Generates the pdict restore epilogue. Must follow the dispatch
    /// result binding (e.g., `let _DispatchResult = ... in`).
    fn pdict_restore_epilogue() -> Document<'static> {
        docvec![
            line(),
            "let _RestoreOk = case _OldState of",
            nest(
                INDENT,
                docvec![
                    line(),
                    "<'undefined'> when 'true' ->",
                    nest(
                        INDENT,
                        docvec![line(), "call 'erlang':'erase'('$bt_actor_state')"]
                    ),
                    line(),
                    "<_Prev> when 'true' ->",
                    nest(
                        INDENT,
                        docvec![line(), "call 'erlang':'put'('$bt_actor_state', _Prev)"]
                    ),
                ]
            ),
            line(),
            "end in",
            line(),
            "let _ClearStack = call 'erlang':'erase'('$bt_call_stack') in",
        ]
    }

    // BT-920: helper — generates the inner `case safe_dispatch ... end` for fire-and-forget casts.
    fn cast_dispatch_case(module_name: &ecow::EcoString) -> Document<'static> {
        docvec![
            // BT-1325: Stash State for re-entrant self-sends
            Self::pdict_stash_preamble(),
            line(),
            // Use safe_dispatch for error isolation; discard result on error
            docvec![
                "let _CastDispatchResult = call '",
                Document::Eco(module_name.clone()),
                "':'safe_dispatch'(CastSelector, CastArgs, State) in"
            ],
            // BT-1325: Restore pdict
            Self::pdict_restore_epilogue(),
            line(),
            "case _CastDispatchResult of",
            nest(
                INDENT,
                docvec![
                    line(),
                    "<{'reply', _CastResult, CastNewState}> when 'true' ->",
                    nest(INDENT, docvec![line(), "{'noreply', CastNewState}"]),
                    line(),
                    // BT-943: Log error but don't crash — caller expects no reply
                    // BT-1822: Destructure error triple to log stacktrace
                    "<{'error', {CastType, CastReason, CastStacktrace}, _CastState}> when 'true' ->",
                    nest(
                        INDENT,
                        docvec![
                            line(),
                            "let CastErrMsg = call 'beamtalk_error':'format_safe'({CastType, CastReason}, CastStacktrace) in",
                            line(),
                            "let _ = call 'logger':'warning'(CastErrMsg, ~{'selector' => CastSelector, 'reason' => {CastType, CastReason}, 'stacktrace' => CastStacktrace, 'domain' => ['beamtalk'|['runtime'|[]]]}~)",
                            line(),
                            "in {'noreply', State}",
                        ]
                    ),
                    line(),
                    // Fallback: plain {error, Error, State} from dispatch (DNU, #beamtalk_error{}, etc.)
                    "<{'error', CastError, _CastState2}> when 'true' ->",
                    nest(
                        INDENT,
                        docvec![
                            line(),
                            "let CastErrMsg2 = call 'beamtalk_error':'format_safe'(CastError) in",
                            line(),
                            "let _ = call 'logger':'warning'(CastErrMsg2, ~{'selector' => CastSelector, 'reason' => CastError, 'domain' => ['beamtalk'|['runtime'|[]]]}~)",
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

    /// BT-1604 (ADR 0069 Phase 2b): Matches 4-tuple messages with `PropCtx`
    /// for both fire-and-forget casts and async sends. Falls back to 3-tuple
    /// for backward compatibility.
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
                            // BT-1604: Fire-and-forget cast with propagated context
                            line(),
                            "<{'cast', CastSelector, CastArgs, CastPropCtx}> when 'true' ->",
                            nest(
                                INDENT,
                                docvec![
                                    line(),
                                    "let _CastCtxOk = call 'beamtalk_actor':'restore_propagated_ctx'(CastPropCtx) in",
                                    Self::cast_dispatch_case(&module_name),
                                ]
                            ),
                            // BT-920: Fire-and-forget cast without context (backward compat)
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
    ///
    /// BT-1604 (ADR 0069 Phase 2b): Matches 3-tuple `{Selector, Args, PropCtx}`
    /// to restore propagated context (`OTel` trace context) before dispatch.
    /// Falls back to 2-tuple `{Selector, Args}` for backward compatibility.
    #[allow(clippy::unnecessary_wraps)] // uniform Result<Document> codegen interface
    pub(in crate::codegen::core_erlang) fn generate_handle_call(
        &mut self,
    ) -> Result<Document<'static>> {
        let module_name = self.module_name.clone();
        let dispatch_case = Self::handle_call_dispatch_case(&module_name);
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
                            // BT-1604: 3-tuple with propagated context — restore before dispatch
                            line(),
                            "<{Selector, Args, PropCtx}> when 'true' ->",
                            nest(
                                INDENT,
                                docvec![
                                    line(),
                                    "let _CtxOk = call 'beamtalk_actor':'restore_propagated_ctx'(PropCtx) in",
                                    dispatch_case.clone(),
                                ]
                            ),
                            // Backward compat: 2-tuple without context
                            line(),
                            "<{Selector, Args}> when 'true' ->",
                            nest(INDENT, dispatch_case),
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

    /// BT-1604: Generates the inner `case safe_dispatch ... end` block for `handle_call`.
    /// Shared between 3-tuple (with `PropCtx`) and 2-tuple (backward compat) patterns.
    fn handle_call_dispatch_case(module_name: &ecow::EcoString) -> Document<'static> {
        docvec![
            // BT-1325: Stash State for re-entrant self-sends
            Self::pdict_stash_preamble(),
            line(),
            // Use safe_dispatch for error isolation per BT-29
            docvec![
                "let _DispatchResult = call '",
                Document::Eco(module_name.clone()),
                "':'safe_dispatch'(Selector, Args, State) in"
            ],
            // BT-1325: Restore pdict
            Self::pdict_restore_epilogue(),
            line(),
            "case _DispatchResult of",
            nest(
                INDENT,
                docvec![
                    // Success case: return {ok, Result}
                    line(),
                    "<{'reply', Result, NewState}> when 'true' ->",
                    nest(
                        INDENT,
                        docvec![line(), "{'reply', {'ok', Result}, NewState}",]
                    ),
                    // Error case: pass error (now includes stacktrace) opaquely to caller
                    line(),
                    "<{'error', Error, ErrState}> when 'true' ->",
                    nest(
                        INDENT,
                        docvec![line(), "{'reply', {'error', Error}, ErrState}",]
                    ),
                ]
            ),
            line(),
            "end",
        ]
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
    ///         <{'error', {_InfoType, InfoReason, InfoStacktrace}, _ErrState}> when 'true' ->
    ///             let _Log = call 'logger':'warning'(Msg, #{stacktrace => InfoStacktrace, ...})
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
                        // BT-1325: Stash State for re-entrant self-sends
                        Self::pdict_stash_preamble(),
                        line(),
                        docvec![
                            "let _InfoDispatchResult = call '",
                            Document::Eco(module_name),
                            "':'safe_dispatch'('handleInfo:', [Msg], State) in",
                        ],
                        Self::pdict_restore_epilogue(),
                        line(),
                        "case _InfoDispatchResult of",
                        nest(
                            INDENT,
                            docvec![
                                line(),
                                "<{'reply', _Result, NewState}> when 'true' ->",
                                nest(INDENT, docvec![line(), "{'noreply', NewState}",]),
                                line(),
                                // BT-1822: Destructure error triple to log stacktrace
                                "<{'error', {InfoType, InfoReason, InfoStacktrace}, _ErrState}> when 'true' ->",
                                nest(
                                    INDENT,
                                    docvec![
                                        line(),
                                        "let InfoErrMsg = call 'beamtalk_error':'format_safe'({InfoType, InfoReason}, InfoStacktrace) in",
                                        line(),
                                        "let _Log = call 'logger':'warning'(InfoErrMsg, ~{'selector' => 'handleInfo:', 'reason' => {InfoType, InfoReason}, 'stacktrace' => InfoStacktrace, 'domain' => ['beamtalk'|['runtime'|[]]]}~)",
                                        line(),
                                        "in {'noreply', State}",
                                    ]
                                ),
                                line(),
                                // Fallback: plain {error, Error, State} from dispatch
                                "<{'error', InfoError, _ErrState2}> when 'true' ->",
                                nest(
                                    INDENT,
                                    docvec![
                                        line(),
                                        "let InfoErrMsg2 = call 'beamtalk_error':'format_safe'(InfoError) in",
                                        line(),
                                        "let _Log = call 'logger':'warning'(InfoErrMsg2, ~{'selector' => 'handleInfo:', 'reason' => InfoError, 'domain' => ['beamtalk'|['runtime'|[]]]}~)",
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
        module: &Module,
    ) -> Result<Document<'static>> {
        let module_name = self.module_name.clone();

        // BT-1642: Use the clean Beamtalk class name for lifecycle telemetry
        // metadata, matching how dispatch traces report class names.
        let current_class = module.classes.iter().find(|c| {
            use super::super::util::module_matches_class;
            module_matches_class(&self.module_name, &c.name.name)
        });
        let class_name = current_class.map_or_else(|| module_name.clone(), |c| c.name.name.clone());

        let doc = docvec![
            "'terminate'/2 = fun (Reason, State) ->",
            nest(
                INDENT,
                docvec![
                    line(),
                    // BT-1638: Emit lifecycle stop telemetry from compiled terminate
                    "let _TelPid = call 'erlang':'self'() in",
                    line(),
                    docvec![
                        "let _TelStop = call 'beamtalk_actor':'maybe_execute_telemetry'(",
                        "['beamtalk', 'actor', 'lifecycle', 'stop'], ~{}~, ~{'pid' => _TelPid, 'class' => '",
                        Document::String(class_name.to_string()),
                        "', 'reason' => Reason}~) in",
                    ],
                    line(),
                    "%% Call terminate: method if defined — exceptions must not prevent shutdown",
                    line(),
                    "let Self = call 'beamtalk_actor':'make_self'(State) in",
                    line(),
                    docvec![
                        "let _TermDisp = try call '",
                        Document::String(module_name.to_string()),
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
