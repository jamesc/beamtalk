// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! OTP `gen_server` callback code generation.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! Generates the standard OTP callbacks: `init/1`, `handle_continue/2`,
//! `handle_cast/2`, `handle_call/3`, `handle_info/2`, `code_change/3`,
//! and `terminate/2`.

use super::super::actor_state_access::{actor_state_call, actor_state_helper};
use super::super::errors::{BeamtalkErrorChain, ErrorBinding};
use super::super::exception_forms::{CatchVars, build_caught_triple_doc, wrap_and_reraise_doc};
use super::super::{CoreErlangGenerator, Result};
use crate::core_erlang::sentinel_tags;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::form::{self, Layout};
use beamtalk_cerl_doc::{Document, INDENT, leaf, line, nest};
use beamtalk_core::ast::{ClassDefinition, Expression, Module, SlotKind};
use ecow::EcoString;

/// A Block-layout `let var = value in`, with the rest of the sequence on the
/// next line.
fn bind(
    var: Document<'static>,
    value: Document<'static>,
    body: Document<'static>,
) -> Document<'static> {
    form::let_in(var, value, body, Layout::Block)
}

/// A Block-layout `case`: one clause per line between `of` and `end`.
fn case_block(
    scrutinee: Document<'static>,
    clauses: impl IntoIterator<Item = Document<'static>>,
) -> Document<'static> {
    form::case_of(scrutinee, clauses, Layout::Block)
}

/// A Block-layout clause with the always-true guard: `<patterns> when 'true' ->`
/// with the body nested beneath.
fn arm(
    patterns: impl IntoIterator<Item = Document<'static>>,
    body: Document<'static>,
) -> Document<'static> {
    form::clause(patterns, None, body, Layout::Block)
}

/// A one-line clause: `<patterns> when 'true' -> body`.
fn arm_inline(
    patterns: impl IntoIterator<Item = Document<'static>>,
    body: Document<'static>,
) -> Document<'static> {
    form::clause(patterns, None, body, Layout::Flat)
}

/// `call 'maps':'get'('__skip_initialize__', InitArgs, 'false')`: the flag a
/// child's `init/1` sets when it calls its parent's as a state-building helper.
fn skip_initialize_flag() -> Document<'static> {
    form::call(
        "maps",
        "get",
        [
            leaf::atom("__skip_initialize__"),
            leaf::var("InitArgs"),
            leaf::atom("false"),
        ],
    )
}

/// The `case` on [`skip_initialize_flag`]: `on_helper` is its `'true'` clause
/// (a parent-helper call), `on_leaf` its `'false'` one.
fn skip_initialize_case(
    on_helper: Document<'static>,
    on_leaf: Document<'static>,
) -> Document<'static> {
    case_block(skip_initialize_flag(), [on_helper, on_leaf])
}

/// `{'noreply', state}`.
fn noreply(state: Document<'static>) -> Document<'static> {
    form::tuple([leaf::atom("noreply"), state])
}

/// ADR 0078: Identifies a single class's `initialize` method in the
/// auto-chained dispatch sequence emitted by `generate_handle_continue`.
#[derive(Debug, Clone)]
pub(in crate::core_erlang) struct InitializeChainEntry {
    /// Compiled Erlang module name for this class (e.g. `"bt@caching_database_actor"`).
    /// The `safe_dispatch` call targets this module so dispatch starts at the
    /// class's own table rather than walking from the leaf.
    pub module_name: String,
}

/// ADR 0078: A typed-no-default state field annotated with the class
/// that declared it, so post-initialize diagnostics can name the owning class
/// rather than always the leaf.
///
/// Values are owned (rather than borrowed from the AST) because
/// cross-file ancestors produce synthetic entries from `ClassInfo` when the
/// defining class's AST is absent from the current compilation unit.
#[derive(Debug, Clone)]
pub(in crate::core_erlang) struct InheritedTypedField {
    /// Beamtalk class name that declared this field.
    pub owning_class: String,
    /// Field (state variable) name.
    pub field_name: String,
    /// Human-readable type annotation (used verbatim in diagnostics).
    pub type_name: String,
}

/// An internal (non-field) key every compiled actor's state map carries.
#[derive(Debug, Clone, Copy)]
enum InternalStateKey {
    /// `'__class_mod__'`: the instance's compiled class module.
    ClassMod,
    /// `'__shape_version__'`: ADR 0123's hot-reload shape version.
    ShapeVersion,
}

impl InternalStateKey {
    fn name(self) -> &'static str {
        match self {
            Self::ClassMod => "__class_mod__",
            Self::ShapeVersion => "__shape_version__",
        }
    }
}

/// The internal keys of an actor's state map, in the order `init/1` writes
/// them. The single source for both the initial state map
/// (`internal_state_prefix_doc`) and the committed-state guard
/// (`clean_state_let`, BT-4113).
const INTERNAL_STATE_KEYS: [InternalStateKey; 2] =
    [InternalStateKey::ClassMod, InternalStateKey::ShapeVersion];

/// BT-4113: the declared fields (the class's and its ancestors') a
/// committed state map of an actor holds when it carries no codegen-internal
/// `__local__` temporaries; with [`INTERNAL_STATE_KEYS`], its whole key set.
/// Built by `committed_state_keys`.
#[derive(Debug)]
struct CommittedStateKeys(Vec<EcoString>);

/// BT-4113: `let <dest> = <src with __local__ temporaries stripped> in`.
///
/// With a known committed key set (`committed_state_keys`), a new state that
/// holds every one of those keys **and** has exactly that many keys holds
/// nothing else, so no `__local__` key: it is kept as is. Anything else (a
/// temporary present, or a field set that differs from the compile-time one,
/// e.g. after hot reload or in a stale subclass of a redefined ancestor)
/// pays for `beamtalk_actor:strip_local_temps/1`'s scan of every key. The
/// check is one map pattern match plus `map_size/1`, with no key-name
/// inspection. Without a key set, it always strips.
///
/// Checking presence as well as size keeps the fast path sound whatever the
/// runtime field set is. A size check alone would let a `__local__` key
/// through whenever the live map had one declared field fewer than the
/// compile-time count, and a stale subclass can: redefining a superclass
/// migrates its loaded subclasses' live instances
/// (`beamtalk_repl_loader_activate:hot_reload_descendants/2`) and their `init/1`
/// calls the new superclass `init/1`, but their own modules are never
/// recompiled.
fn clean_state_let(
    dest: &'static str,
    src: &'static str,
    state_keys: Option<&CommittedStateKeys>,
    body: Document<'static>,
) -> Document<'static> {
    let strip = form::call("beamtalk_actor", "strip_local_temps", [leaf::var(src)]);
    let value = match state_keys {
        None => strip,
        Some(CommittedStateKeys(fields)) => {
            let keys = INTERNAL_STATE_KEYS
                .iter()
                .map(|k| k.name().to_owned())
                .chain(fields.iter().map(ToString::to_string));
            let mut pattern: Vec<Document<'static>> = Vec::new();
            let mut size: i64 = 0;
            for (i, key) in keys.enumerate() {
                if i > 0 {
                    pattern.push(Document::Str(", "));
                }
                pattern.push(leaf::atom(key));
                pattern.push(Document::Str(" := "));
                pattern.push(leaf::var(format!("_Key{i}")));
                size += 1;
            }
            case_block(
                leaf::var(src),
                [
                    form::clause(
                        [docvec!["~{", Document::Vec(pattern), "}~"]],
                        Some(form::call(
                            "erlang",
                            "=:=",
                            [
                                form::call("erlang", "map_size", [leaf::var(src)]),
                                leaf::int_lit(size),
                            ],
                        )),
                        leaf::var(src),
                        Layout::Flat,
                    ),
                    form::clause([leaf::var("_")], None, strip, Layout::Flat),
                ],
            )
        }
    };
    bind(leaf::var(dest), value, body)
}

/// The Core Erlang variable names of one pdict install / uninstall bracket
/// around a `safe_dispatch` (BT-4888): the `State` the install replaced, the
/// uninstall's result, and the call-stack erase's result. The re-entrant
/// self-send brackets use one fixed set; each `handle_continue` chain entry
/// needs its own, because its brackets nest in one function body.
struct PdictVars {
    old: String,
    restore: String,
    clear: String,
}

impl PdictVars {
    /// The single bracket of a `handle_call` / `handle_cast` / continuation
    /// dispatch.
    fn reentrant() -> Self {
        Self {
            old: "_OldState".to_string(),
            restore: "_RestoreOk".to_string(),
            clear: "_ClearStack".to_string(),
        }
    }

    /// The bracket around chain entry `idx`'s `initialize` in
    /// `generate_handle_continue`.
    fn chain_step(idx: &str) -> Self {
        Self {
            old: format!("_OldPdict{idx}"),
            restore: format!("_RestorePdict{idx}"),
            clear: format!("_ClearStack{idx}"),
        }
    }
}

/// The pieces that vary between the three "log the error, then stop" arms
/// of the generated `init/1` (BT-4887).
struct LogAndStop<'a> {
    /// Variable bound to `beamtalk_error:format_safe/1,2`'s message.
    msg_var: String,
    /// Variable bound to the class's `class_name/0`.
    class_name_var: String,
    /// The class module whose `class_name/0` is called.
    module_name: &'a str,
    /// The arguments of `format_safe` (one error, or `{Type, Reason}, Stack`).
    format_safe_args: Document<'static>,
    /// An extra `let <var> = beamtalk_error:format(<arg>) in` between the
    /// class-name and the logger call (the uninitialized-field arm stops with
    /// the formatted error rather than the raw one).
    formatted_var: Option<(String, Document<'static>)>,
    /// The logger's `'reason'` value.
    reason: Document<'static>,
    /// The logger's optional `'stacktrace'` value.
    stacktrace: Option<Document<'static>>,
    /// The first element after `'stop'`.
    stop_reason: Document<'static>,
    /// The state element of the `{'stop', Reason, State}` tuple.
    stop_state: Document<'static>,
}

/// BT-4887: the one "`format_safe` + `class_name` + `logger:error` + `{'stop', …}`"
/// arm shared by the generated `init/1`'s uninitialized-field check and its
/// two `{'error', …}` result clauses.
fn log_and_stop_arm(arm: LogAndStop<'_>) -> Document<'static> {
    let mut log_map = vec![
        Document::Str("~{'class' => "),
        leaf::var(arm.class_name_var.clone()),
        Document::Str(", 'reason' => "),
        arm.reason,
    ];
    if let Some(stack) = arm.stacktrace {
        log_map.push(Document::Str(", 'stacktrace' => "));
        log_map.push(stack);
    }
    log_map.push(Document::Str(", 'domain' => ['beamtalk'|['runtime'|[]]]}~"));
    let stop = form::tuple([leaf::atom("stop"), arm.stop_reason, arm.stop_state]);
    let log = bind(
        leaf::var("_"),
        form::call(
            "logger",
            "error",
            [leaf::var(arm.msg_var.clone()), Document::Vec(log_map)],
        ),
        stop,
    );
    let formatted = match arm.formatted_var {
        Some((var, format_arg)) => bind(
            leaf::var(var),
            form::call("beamtalk_error", "format", [format_arg]),
            log,
        ),
        None => log,
    };
    bind(
        leaf::var(arm.msg_var),
        form::call("beamtalk_error", "format_safe", [arm.format_safe_args]),
        bind(
            leaf::var(arm.class_name_var),
            form::call(arm.module_name, "class_name", []),
            formatted,
        ),
    )
}

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
    ///                 '__shape_version__' => 1,
    ///                 'logCount' => 0
    ///             }~
    ///             in let MergedState = call 'maps':'merge'(ParentState, ChildFields)
    ///             in let CheckedArgs = <BT-4030 unknown-key check, leaf call only>
    ///             in let FinalState = call 'maps':'merge'(MergedState, CheckedArgs)
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
    ///         '__shape_version__' => 1,
    ///         'value' => 0
    ///     }~
    ///     in let CheckedArgs = <BT-4030 unknown-key check>
    ///     in let FinalState = call 'maps':'merge'(DefaultState, CheckedArgs)
    ///        in {'ok', FinalState}
    /// ```
    #[allow(clippy::too_many_lines)] // inheritance-aware init with parent state merge
    pub(in crate::core_erlang) fn generate_init_function(
        &mut self,
        module: &Module,
    ) -> Result<Document<'static>> {
        // Find the current class to check for superclass
        // NOTE: This requires the .bt file to have an explicit class definition
        // like "Counter subclass: LoggingCounter" (see tests/fixtures/logging_counter.bt).
        // Module-level expressions without a class definition take the base class path below.
        let current_class = self.current_class(module);

        // Check if we have a superclass that's not Actor (base class)
        // When true, we'll call the parent's init to inherit state fields
        let has_parent_init = current_class.is_some_and(Self::has_parent_actor_module);

        // Check if the class defines an initialize method.
        // If so, dispatch it at the end of init/1 — but only when not called
        // as a parent state-building helper. The __skip_initialize__ flag in
        // InitArgs suppresses dispatch when a child's init calls us as a helper.
        //
        // ADR 0078: Also defer to handle_continue when any *ancestor*
        // class defines initialize — handle_continue auto-chains each ancestor's
        // initialize parent-first. And defer when any class in the chain has
        // typed-no-default fields so the post-initialize validation runs.
        let chain_has_initialize = current_class.is_some_and(|c| {
            !self
                .user_defined_initialize_chain(module, &c.name.name)
                .is_empty()
        });
        let chain_has_typed_no_default = current_class.is_some_and(|c| {
            !self
                .inherited_typed_no_default_fields(module, &c.name.name)
                .is_empty()
        });
        let has_initialize = chain_has_initialize || chain_has_typed_no_default;

        // BT-3596: trap exits only for classes whose effective `terminate:`
        // is overridden somewhere in the hierarchy — see
        // `class_or_ancestor_overrides_terminate` doc comment. Actors that
        // never override `terminate:` keep today's plain-link semantics and
        // this leaves their `init/1` byte-identical.
        // BT-4052: a Server that overrides `onLinkedExit:reason:` also traps
        // (the hook alone suffices — no dummy `terminate:` needed).
        let traps_exit = current_class.is_some_and(|c| {
            self.class_or_ancestor_overrides_terminate(module, &c.name.name)
                || (self.is_server_subclass
                    && self.class_or_ancestor_defines_linked_exit_hook(module, &c.name.name))
        });

        let module_name = self.module_name.clone();

        // ADR 0123 §1: `'__shape_version__'` is the leaf class's own
        // declared `shapeVersion:` (default 1) — never inherited, since
        // shape versions and migration chains are per concrete class
        // (BT-3534 added the internal-fields plumbing; this is what makes
        // the written value non-default).
        let shape_version = current_class.map_or(1, ClassDefinition::effective_shape_version);

        // Use the clean Beamtalk class name (e.g., "EventStore") for
        // lifecycle telemetry metadata instead of the compiled Erlang module name
        // (e.g., "bt@exdura@event_store"). This matches how dispatch traces
        // report class names via lookup_class/1.
        let class_name = current_class.map_or_else(|| module_name.clone(), |c| c.name.name.clone());

        // Generate the init return — either plain {ok, State} or
        // dispatch initialize first, then return {ok, NewState} / {stop, Error}.
        // When has_initialize is true, we wrap the dispatch in a guard that
        // checks __skip_initialize__ in InitArgs, so parent helpers skip it.
        // Lifecycle start telemetry is emitted here, guarded by
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
                // ADR 0016: Use the same module naming logic as
                // superclass_module_name() which handles stdlib (bt@stdlib@*),
                // package (bt@{pkg}@*), and legacy (bt@*) modules
                self.compiled_module_name(class.superclass_name())
            };

            // Get this class's own state fields
            let own_state_fields = self.generate_own_state_fields(module)?;
            let (own_prelude, own_state_fields) =
                (own_state_fields.prelude, own_state_fields.fields);

            let doc = docvec![
                form::fun_def(
                    "init",
                    [leaf::var("InitArgs")],
                    // ADR 0079: set the `$beamtalk_actor` marker
                    // so `class named:` / `allRegistered` can filter this
                    // process as a Beamtalk actor. Guarded by
                    // `__skip_initialize__` so parent helpers don't overwrite
                    // the leaf class's marker.
                    Self::beamtalk_actor_marker_doc(
                        &class_name,
                        traps_exit,
                        docvec![
                            "%% Call parent init to get inherited state fields",
                            line(),
                            // Pass __skip_initialize__ to parent so it
                            // doesn't dispatch initialize — only the leaf should.
                            bind(
                                leaf::var("_ParentArgs"),
                                form::call(
                                    "maps",
                                    "put",
                                    [
                                        leaf::atom("__skip_initialize__"),
                                        leaf::atom("true"),
                                        leaf::var("InitArgs"),
                                    ],
                                ),
                                case_block(
                                    form::call(parent_module, "init", [leaf::var("_ParentArgs")]),
                                    [
                                        arm(
                                            [form::tuple([
                                                leaf::atom("ok"),
                                                leaf::var("ParentState"),
                                            ])],
                                            docvec![
                                                "%% Merge parent state with the fields declared by this class",
                                                line(),
                                                own_prelude,
                                                // The `in let` chain keeps `in` at the start of each
                                                // line, which no `form::let_in` layout produces.
                                                "let ChildFields = ~{",
                                                nest(
                                                    INDENT,
                                                    docvec![
                                                        line(),
                                                        Self::internal_state_prefix_doc(
                                                            &module_name,
                                                            shape_version
                                                        ),
                                                        Document::Vec(own_state_fields),
                                                    ]
                                                ),
                                                line(),
                                                "}~",
                                                line(),
                                                "in let MergedState = ",
                                                form::call(
                                                    "maps",
                                                    "merge",
                                                    [
                                                        leaf::var("ParentState"),
                                                        leaf::var("ChildFields"),
                                                    ],
                                                ),
                                                line(),
                                                // Merge InitArgs last so user-provided values override defaults
                                                // Order: parent defaults → child defaults → user overrides
                                                // `self.class_name()` (not the AST-or-module
                                                // `class_name` above): identical when a class is
                                                // present, and consistent with the spawn arms
                                                // for expression modules with no class.
                                                Self::check_init_args_guarded_doc(
                                                    &self.class_name(),
                                                    "MergedState",
                                                ),
                                                line(),
                                                "in let FinalState = ",
                                                form::call(
                                                    "maps",
                                                    "merge",
                                                    [
                                                        leaf::var("MergedState"),
                                                        leaf::var("CheckedArgs"),
                                                    ],
                                                ),
                                                line(),
                                                init_return.clone(),
                                            ]
                                        ),
                                        arm(
                                            [form::tuple([
                                                leaf::atom("error"),
                                                leaf::var("Reason"),
                                            ])],
                                            docvec![
                                                "%% Propagate parent init error",
                                                line(),
                                                form::tuple([
                                                    leaf::atom("error"),
                                                    leaf::var("Reason")
                                                ]),
                                            ]
                                        ),
                                    ],
                                ),
                            ),
                        ],
                    ),
                    Layout::Block,
                ),
                "\n",
                "\n",
            ];
            Ok(doc)
        } else {
            // No parent, or parent is Actor base class - generate normal init
            let initial_state_fields = self.generate_initial_state_fields(module)?;
            let (initial_prelude, initial_state_fields) =
                (initial_state_fields.prelude, initial_state_fields.fields);

            let doc = docvec![
                form::fun_def(
                    "init",
                    [leaf::var("InitArgs")],
                    // ADR 0079: set the `$beamtalk_actor` marker
                    // so `class named:` / `allRegistered` can filter this
                    // process as a Beamtalk actor. Guarded by
                    // `__skip_initialize__` so parent helpers don't overwrite
                    // the leaf class's marker.
                    Self::beamtalk_actor_marker_doc(
                        &class_name,
                        traps_exit,
                        docvec![
                            initial_prelude,
                            // The `in let` chain keeps `in` at the start of each
                            // line, which no `form::let_in` layout produces.
                            "let DefaultState = ~{",
                            nest(
                                INDENT,
                                docvec![
                                    line(),
                                    Self::internal_state_prefix_doc(&module_name, shape_version),
                                    Document::Vec(initial_state_fields),
                                ]
                            ),
                            line(),
                            "}~",
                            line(),
                            // Merge InitArgs into DefaultState - InitArgs values override defaults
                            // `self.class_name()`: see the MergedState path above.
                            Self::check_init_args_guarded_doc(&self.class_name(), "DefaultState"),
                            line(),
                            "in let FinalState = ",
                            form::call(
                                "maps",
                                "merge",
                                [leaf::var("DefaultState"), leaf::var("CheckedArgs")],
                            ),
                            line(),
                            init_return,
                        ],
                    ),
                    Layout::Block,
                ),
                "\n",
                "\n",
            ];
            Ok(doc)
        }
    }

    /// BT-4030: the leaf-only `spawnWith:` unknown-key check.
    ///
    /// Binds `CheckedArgs` — `InitArgs` validated via `check_init_args/5`
    /// against the class's full declared field set (own, inherited, and
    /// `late`, resolved from metadata at runtime), raising `#unknown_field`
    /// for undeclared keys. Guarded by `__skip_initialize__` like the
    /// marker and `init_return` below: a parent-helper call carries the
    /// leaf's own keys and skips, leaving validation to the leaf. The
    /// `'$bt_init_ack'` protocol key rides in `ExtraAllowed` since the
    /// spawner adds it to every map's start args.
    ///
    /// Let-chain convention: opens with `in` (closing the base-state
    /// binding) and ends with a bare `end` (the following `in let
    /// FinalState` closes this one) — mirroring the neighbouring
    /// `in let MergedState` / `in let FinalState` items.
    fn check_init_args_guarded_doc(
        class_name: &str,
        base_state_var: &'static str,
    ) -> Document<'static> {
        docvec![
            "in let CheckedArgs = ",
            skip_initialize_case(
                arm_inline([leaf::atom("true")], leaf::var("InitArgs")),
                arm(
                    [leaf::atom("false")],
                    super::super::errors::check_init_args_call_doc(
                        leaf::atom(class_name.to_owned()),
                        "spawnWith:",
                        Document::Str(base_state_var),
                        Document::Str("InitArgs"),
                        &[sentinel_tags::INIT_ACK],
                    ),
                ),
            ),
        ]
    }

    /// The `'__class_mod__'` and `'__shape_version__' => N` internal-key
    /// pair, as map-literal entries (each after the first needs its own
    /// leading `, `) — the prefix shared, byte for byte, by both `init/1`
    /// code paths: the base-class `DefaultState` map and the subclass
    /// `ChildFields` map both start every internal-key block with these
    /// same two entries against the same `module_name`.
    ///
    /// `'__shape_version__'` is ADR 0123 Phase 0's (BT-3534) hot-reload
    /// shape version key; `shape_version` is this leaf class's own
    /// effective `shapeVersion:` (ADR 0123 §1, default `1`) — never the
    /// superclass's, since shape versions are per concrete class.
    ///
    /// The entries come from [`INTERNAL_STATE_KEYS`], the same table the
    /// committed-state guard ([`clean_state_let`]) matches, so the guard
    /// can never disagree with the map `init/1` builds.
    fn internal_state_prefix_doc(module_name: &str, shape_version: u32) -> Document<'static> {
        let mut parts: Vec<Document<'static>> = Vec::new();
        for (i, key) in INTERNAL_STATE_KEYS.iter().enumerate() {
            if i > 0 {
                parts.push(line());
                parts.push(Document::Str(", "));
            }
            parts.push(leaf::atom(key.name()));
            parts.push(Document::Str(" => "));
            parts.push(match key {
                InternalStateKey::ClassMod => leaf::atom(module_name.to_owned()),
                InternalStateKey::ShapeVersion => leaf::int_lit(i64::from(shape_version)),
            });
        }
        Document::Vec(parts)
    }

    /// BT-4113: every declared field of the class and its ancestors: with
    /// [`INTERNAL_STATE_KEYS`], the key set a committed state map of this
    /// actor holds when it carries no codegen-internal `__local__`
    /// temporaries. The generated callbacks skip the `strip_local_temps/1`
    /// key scan when the new state holds all of these keys and no others
    /// (see [`clean_state_let`]).
    ///
    /// The guard checks presence as well as count, so it stays sound
    /// whatever the runtime field set turns out to be: an ancestor missing
    /// from the compile-time hierarchy, a superclass redefined without this
    /// module being recompiled, or a hot-reload shape migration only make it
    /// fall back to the scan. This answers `None` (always strip) where the
    /// key set is unknown or the fast path could rarely match: no class
    /// identity or hierarchy, a script module seeding extra keys from
    /// top-level assignments, or a `late` slot anywhere in the chain (a
    /// defaultless one is absent until first assigned).
    fn committed_state_keys(&self, module: &Module) -> Option<CommittedStateKeys> {
        if module
            .expressions
            .iter()
            .any(|stmt| matches!(stmt.expression, Expression::Assignment { .. }))
        {
            return None;
        }
        let class = self.current_class(module)?;
        let hierarchy = self.class_hierarchy.as_ref()?;
        let class_name = class.name.name.as_str();
        hierarchy.get_class(class_name)?;
        let fields = hierarchy.all_state(class_name);
        if fields
            .iter()
            .any(|f| hierarchy.state_field_kind(class_name, f) == SlotKind::Late)
        {
            return None;
        }
        Some(CommittedStateKeys(fields))
    }

    /// ADR 0079: Generate the `$beamtalk_actor` process-dictionary
    /// marker call. Every Beamtalk actor init/1 sets this so `all_registered/0`,
    /// `class named:`, and future tooling can distinguish compiled Beamtalk
    /// actors from raw OTP processes via `erlang:process_info(Pid, dictionary)`.
    ///
    /// Guarded by `__skip_initialize__` so that when a subclass's init/1 calls
    /// the parent as a state-building helper, the parent does not overwrite
    /// the subclass's marker — the leaf class name is what downstream lookups
    /// need.
    ///
    /// BT-4937: the same branch also writes `'$beamtalk_actor_started_at'`
    /// (`erlang:system_time(microsecond)`), the age `resolve_global_conflict/3`
    /// compares. It is the only writer of that key now that the interpretive
    /// `beamtalk_actor:init/1` is going away.
    ///
    /// BT-3596: when `traps_exit` is true (this class or an ancestor
    /// overrides `terminate:`, see `class_or_ancestor_overrides_terminate`),
    /// also sets `trap_exit` on this actor's own `gen_server` process — in the
    /// same `__skip_initialize__`-guarded, non-helper branch, so it fires
    /// exactly once per spawned process and not on every parent-helper
    /// recursion. Without this, a supervisor's `exit(Pid, shutdown)` (used by
    /// both `terminate_child/2` and `rest_for_one`/`one_for_all` cascades)
    /// kills the process immediately and `terminate/2` never runs. When
    /// `traps_exit` is false, this is byte-identical to the pre-BT-3596
    /// output — actors that never override `terminate:` keep today's plain
    /// link semantics.
    fn beamtalk_actor_marker_doc(
        class_name: &str,
        traps_exit: bool,
        body: Document<'static>,
    ) -> Document<'static> {
        let put_marker = form::call(
            "erlang",
            "put",
            [
                leaf::atom("$beamtalk_actor"),
                leaf::atom(class_name.to_owned()),
            ],
        );
        // BT-4937 / ADR 0126 §4: the start-time marker
        // `beamtalk_actor_registry:resolve_global_conflict/3` reads (via
        // `actor_started_at/1`, possibly from another node) to keep the older
        // registrant. Written once per spawned process, in the same
        // `__skip_initialize__`-guarded branch as the class marker.
        let put_started_at = form::call(
            "erlang",
            "put",
            [
                leaf::atom("$beamtalk_actor_started_at"),
                form::call("erlang", "system_time", [leaf::atom("microsecond")]),
            ],
        );
        let put_markers = bind(leaf::var("_ClassMarker"), put_marker, put_started_at);
        let false_arm_body = if traps_exit {
            bind(
                leaf::var("_TrapExit"),
                form::call(
                    "erlang",
                    "process_flag",
                    [leaf::atom("trap_exit"), leaf::atom("true")],
                ),
                put_markers,
            )
        } else {
            put_markers
        };
        bind(
            leaf::var("_Marker"),
            skip_initialize_case(
                arm_inline([leaf::atom("true")], leaf::atom("skipped")),
                arm([leaf::atom("false")], false_arm_body),
            ),
            body,
        )
    }

    /// Generate the lifecycle start telemetry call for init/1.
    ///
    /// Emits `[beamtalk, actor, lifecycle, start]` via `beamtalk_actor:maybe_execute_telemetry/3`.
    /// Uses a `let` binding for `self()` since Core Erlang map literals cannot contain calls.
    /// Takes the clean Beamtalk class name (e.g., `EventStore`), not the
    /// compiled module name, so lifecycle traces match dispatch trace format.
    fn lifecycle_telemetry_let(
        event: &'static str,
        telemetry_var: &'static str,
        class_name: &str,
        extra_metadata: Document<'static>,
        body: Document<'static>,
    ) -> Document<'static> {
        bind(
            leaf::var("_TelPid"),
            form::call("erlang", "self", []),
            bind(
                leaf::var(telemetry_var),
                form::call(
                    "beamtalk_actor",
                    "maybe_execute_telemetry",
                    [
                        docvec![
                            "['beamtalk', 'actor', 'lifecycle', ",
                            leaf::atom(event),
                            "]"
                        ],
                        Document::Str("~{}~"),
                        docvec![
                            "~{'pid' => _TelPid, 'class' => ",
                            leaf::atom(class_name.to_owned()),
                            extra_metadata,
                            "}~"
                        ],
                    ],
                ),
                body,
            ),
        )
    }

    /// Generate a plain init return with lifecycle telemetry.
    ///
    /// For classes WITHOUT `initialize`, emits lifecycle start telemetry
    /// guarded by `__skip_initialize__` so parent helper calls don't double-fire.
    /// When called as a parent helper (flag is 'true'), skips telemetry and
    /// returns `{ok, State}` directly.
    fn init_plain_return_doc(class_name: &str) -> Document<'static> {
        docvec![
            // Guard telemetry behind __skip_initialize__ to avoid
            // double-fire when called as parent state-building helper
            "in ",
            skip_initialize_case(
                arm(
                    [leaf::atom("true")],
                    bind(
                        leaf::var("CleanState"),
                        form::call(
                            "maps",
                            "remove",
                            [leaf::atom("__skip_initialize__"), leaf::var("FinalState")],
                        ),
                        form::tuple([leaf::atom("ok"), leaf::var("CleanState")]),
                    ),
                ),
                arm(
                    [leaf::atom("false")],
                    Self::lifecycle_telemetry_let(
                        "start",
                        "_TelStart",
                        class_name,
                        Document::Nil,
                        // BT-3992: strip the starter's initialize-ack
                        // target and ack at once — no `initialize` step
                        // follows, so startup ends here.
                        bind(
                            leaf::var("AckState"),
                            form::call(
                                "beamtalk_actor",
                                "take_init_ack",
                                [leaf::var("FinalState"), leaf::atom("initialized")],
                            ),
                            form::tuple([leaf::atom("ok"), leaf::var("AckState")]),
                        ),
                    ),
                ),
            ),
        ]
    }

    /// Generate the guarded initialize dispatch block for init/1.
    ///
    /// Checks `__skip_initialize__` in `InitArgs` — when a child's init calls
    /// us as a parent state helper, it sets this flag to prevent double dispatch.
    /// When called as the outermost init (by OTP), the flag is absent and
    /// initialize is deferred to `handle_continue`.
    ///
    /// Uses OTP's `handle_continue` pattern instead of dispatching
    /// initialize inline. This ensures the `gen_server` message loop is running
    /// when initialize executes, so self-sends don't deadlock. OTP guarantees
    /// no messages arrive before `handle_continue` runs.
    ///
    /// Also emits lifecycle start telemetry in the non-helper branch.
    fn init_initialize_guarded_doc(class_name: &str) -> Document<'static> {
        docvec![
            "%% BT-1417/BT-1541: Defer initialize to handle_continue unless called as a parent helper",
            line(),
            "in ",
            skip_initialize_case(
                arm(
                    [leaf::atom("true")],
                    // Strip the flag from state before returning
                    bind(
                        leaf::var("CleanState"),
                        form::call(
                            "maps",
                            "remove",
                            [leaf::atom("__skip_initialize__"), leaf::var("FinalState")],
                        ),
                        form::tuple([leaf::atom("ok"), leaf::var("CleanState")]),
                    ),
                ),
                arm(
                    [leaf::atom("false")],
                    // Strip the flag from state before returning
                    bind(
                        leaf::var("CleanState1"),
                        form::call(
                            "maps",
                            "remove",
                            [leaf::atom("__skip_initialize__"), leaf::var("FinalState")],
                        ),
                        // Emit lifecycle start telemetry before returning
                        Self::lifecycle_telemetry_let(
                            "start",
                            "_TelStart",
                            class_name,
                            Document::Nil,
                            // BT-3992: strip the starter's initialize-ack
                            // target and keep it for handle_continue's
                            // `initialize` step, which acks once it succeeds.
                            bind(
                                leaf::var("AckState"),
                                form::call(
                                    "beamtalk_actor",
                                    "take_init_ack",
                                    [leaf::var("CleanState1"), leaf::atom("initializing")],
                                ),
                                // Return {ok, State, {continue, initialize}} to defer
                                // initialize dispatch to handle_continue where the loop is running
                                form::tuple([
                                    leaf::atom("ok"),
                                    leaf::var("AckState"),
                                    form::tuple([leaf::atom("continue"), leaf::atom("initialize")]),
                                ]),
                            ),
                        ),
                    ),
                ),
            ),
        ]
    }

    /// Generates the success body for `handle_continue` after
    /// the auto-chained initialize sequence.
    ///
    /// Collects typed-no-default state fields from the full inheritance chain
    /// (ADR 0078). For each such field that is still `nil` after the chain
    /// completes, raises `UninitializedStateError` with the owning class name,
    /// field name, and expected type. If no such fields exist, returns a plain
    /// `{'noreply', InitNewState}`.
    #[allow(clippy::too_many_lines)]
    fn generate_post_initialize_check(
        &self,
        current_class: Option<&ClassDefinition>,
        module: &Module,
        state_keys: Option<&CommittedStateKeys>,
        // Reserved for future log-metadata enrichment that names the leaf
        // spawning class in addition to the field's owning class.
        _leaf_class_name: &ecow::EcoString,
    ) -> Document<'static> {
        // Collect typed-no-default fields from the full inheritance
        // chain (leaf + ancestors), not just the current class.
        //
        // Ancestors defined outside this module fall back to
        // `ClassInfo` metadata so cross-file typed-no-default fields are
        // validated too.
        let typed_no_default: Vec<InheritedTypedField> = current_class
            .map(|c| self.inherited_typed_no_default_fields(module, &c.name.name))
            .unwrap_or_default();

        if typed_no_default.is_empty() {
            // strip codegen-internal `__local__` threading temps from the
            // post-initialize committed state, the same outermost-boundary clean-up
            // handle_call/handle_cast apply — an `initialize` that threads an outer
            // local must not persist the temporary into the actor's first state.
            return clean_state_let(
                "InitCleanState",
                "InitNewState",
                state_keys,
                noreply(leaf::var("InitCleanState")),
            );
        }

        // Build nested checks: each field gets a case that either stops or continues.
        // We build from the inside out — the innermost is the cleaned {'noreply', …}
        // (__local__ threading temps stripped from the committed state).
        let mut body: Document<'static> = noreply(leaf::var("InitCleanState"));

        for (i, field) in typed_no_default.iter().enumerate().rev() {
            let field_name = field.field_name.clone();
            let type_name = field.type_name.clone();
            // Use the owning class name (which may be an ancestor)
            // in the diagnostic, while keeping the leaf class in logger metadata
            // so operators see which actor failed to start.
            let owning_class = field.owning_class.as_str();
            let idx = i.to_string();
            let check_var = format!("_ChkVal{idx}");
            let err_var0 = format!("_UErr{idx}a");
            let err_var1 = format!("_UErr{idx}b");
            let err_msg_var = format!("_UErrMsg{idx}");
            let class_name_var = format!("_UErrClass{idx}");
            let fmt_var = format!("_UFmt{idx}");
            let hint_msg = format!(
                "{owning_class} field '{field_name}' (:: {type_name}) was not initialized",
            );

            body = bind(
                leaf::var(check_var.clone()),
                form::call(
                    "maps",
                    "get",
                    [leaf::atom(field_name), leaf::var("InitNewState")],
                ),
                case_block(
                    leaf::var(check_var),
                    [
                        arm(
                            [leaf::atom("nil")],
                            docvec![
                                BeamtalkErrorChain::new(
                                    "uninitialized_state_error",
                                    leaf::atom(owning_class.to_string())
                                )
                                .hint(leaf::binary_lit(&hint_msg))
                                .build(
                                    vec![
                                        ErrorBinding::plain(err_var0),
                                        ErrorBinding::plain(err_var1.clone()),
                                    ],
                                    line(),
                                ),
                                // terminating path — the state flows to
                                // terminate/2, not persistence, so no clean-up contract
                                // applies; use the cleaned binding for consistency with
                                // the success arm.
                                log_and_stop_arm(LogAndStop {
                                    msg_var: err_msg_var,
                                    class_name_var,
                                    module_name: &self.module_name,
                                    format_safe_args: leaf::var(err_var1.clone()),
                                    formatted_var: Some((
                                        fmt_var.clone(),
                                        leaf::var(err_var1.clone())
                                    )),
                                    reason: leaf::var(err_var1),
                                    stacktrace: None,
                                    stop_reason: leaf::var(fmt_var),
                                    stop_state: Document::Str("InitCleanState"),
                                }),
                            ],
                        ),
                        arm([leaf::var("_")], body),
                    ],
                ),
            );
        }

        // clean the committed state once before the field checks; the
        // checks read user fields (never `__local__`) so they are unaffected.
        clean_state_let(
            "InitCleanState",
            "InitNewState",
            state_keys,
            docvec![
                "%% BT-1949/BT-1951: Verify typed-no-default fields (including inherited) were initialized",
                line(),
                body,
            ],
        )
    }

    /// ADR 0078: Returns the list of user-defined initializers to run
    /// for a class, parent-first.
    ///
    /// Each entry identifies a class in the inheritance chain (leaf last) that
    /// defines its own `initialize` method. Root classes (`Actor`, `Object`,
    /// `ProtoObject`) are excluded because their `initialize` is a no-op default
    /// — running them would generate unnecessary dispatch calls.
    ///
    /// Uses the compile-time `ClassHierarchy` snapshot so cross-file ancestors
    /// are visible. When the hierarchy is unavailable (e.g. generator
    /// constructed standalone without `generate_module_with_warnings`), falls
    /// back to the leaf class's own `initialize` method if the AST defines one
    /// so single-class initialization still runs.
    pub(in crate::core_erlang) fn user_defined_initialize_chain(
        &self,
        module: &Module,
        leaf_class: &str,
    ) -> Vec<InitializeChainEntry> {
        let Some(hierarchy) = self.class_hierarchy.as_ref() else {
            // Fallback: AST only, leaf class initialize.
            return module
                .classes
                .iter()
                .find(|c| c.name.name == leaf_class)
                .filter(|c| c.methods.iter().any(|m| m.selector.name() == "initialize"))
                .map(|c| {
                    vec![InitializeChainEntry {
                        module_name: self.compiled_module_name(&c.name.name),
                    }]
                })
                .unwrap_or_default();
        };

        // Build the chain parent-first: leaf's superclass_chain returns
        // immediate ancestors first (Actor, Object, ProtoObject at the end
        // after user classes). Reverse to put the root ancestor first, then
        // append the leaf.
        let mut ordered: Vec<ecow::EcoString> = hierarchy
            .superclass_chain(leaf_class)
            .into_iter()
            .rev()
            .collect();
        ordered.push(ecow::EcoString::from(leaf_class));

        let mut out = Vec::new();
        for name in ordered {
            // Skip root no-op initialize owners — Actor's `initialize` is a
            // no-op per ADR 0078 and we don't generate a dispatch for it.
            if matches!(name.as_str(), "Actor" | "Object" | "ProtoObject") {
                continue;
            }
            let defines_initialize = hierarchy
                .get_class(name.as_str())
                .is_some_and(|info| info.methods.iter().any(|m| m.selector == "initialize"));
            if !defines_initialize {
                continue;
            }
            let module_name = self.compiled_module_name(name.as_str());
            out.push(InitializeChainEntry { module_name });
        }
        out
    }

    /// BT-3596: Returns true when `leaf_class`, or an ancestor other than
    /// `Actor`/`Object`/`ProtoObject`, defines its own `terminate:` method —
    /// i.e. overrides `Actor`'s no-op default `terminate:`.
    ///
    /// Drives the `trap_exit` gating in `generate_init_function`: an actor
    /// whose effective `terminate:` is never overridden keeps today's plain
    /// link semantics (killed immediately by a linked exit, `terminate/2`
    /// never runs), while one that does override it traps exits so OTP
    /// delivers a supervisor's `exit(Pid, shutdown)` — used by both
    /// `terminate_child/2` and `rest_for_one`/`one_for_all` cascades — as a
    /// message instead of killing the process outright, letting the normal
    /// parent-EXIT path in the generated `gen_server` loop call
    /// `terminate/2` before exiting.
    ///
    /// Same AST-only fallback as `user_defined_initialize_chain`: when the
    /// compile-time `ClassHierarchy` snapshot is unavailable, falls back to
    /// the leaf class's own AST methods so single-class compilation (e.g.
    /// the REPL, or a generator built without `generate_module_with_warnings`)
    /// still detects an override — it just can't see ancestors defined in
    /// other files.
    pub(in crate::core_erlang) fn class_or_ancestor_overrides_terminate(
        &self,
        module: &Module,
        leaf_class: &str,
    ) -> bool {
        self.class_or_ancestor_defines_method(
            module,
            leaf_class,
            "terminate:",
            &["Actor", "Object", "ProtoObject"],
        )
    }

    /// BT-4052: Returns true when `leaf_class`, or an ancestor other than
    /// `Server`/`Actor`/`Object`/`ProtoObject`, defines its own
    /// `onLinkedExit:reason:` method — i.e. overrides `Server`'s no-op default.
    ///
    /// Drives both the `trap_exit` gating in `generate_init_function` (for
    /// Server subclasses) and the baked flag passed to
    /// `beamtalk_actor:handle_linked_exit/3` in `generate_handle_info`. Same
    /// AST-only no-hierarchy fallback as `class_or_ancestor_overrides_terminate`.
    pub(in crate::core_erlang) fn class_or_ancestor_defines_linked_exit_hook(
        &self,
        module: &Module,
        leaf_class: &str,
    ) -> bool {
        self.class_or_ancestor_defines_method(
            module,
            leaf_class,
            "onLinkedExit:reason:",
            &["Server", "Actor", "Object", "ProtoObject"],
        )
    }

    /// Shared walk for the override predicates above: does `leaf_class` or an
    /// ancestor not in `excluded_defaults` define `selector`?
    fn class_or_ancestor_defines_method(
        &self,
        module: &Module,
        leaf_class: &str,
        selector: &str,
        excluded_defaults: &[&str],
    ) -> bool {
        let Some(hierarchy) = self.class_hierarchy.as_ref() else {
            return module
                .classes
                .iter()
                .find(|c| c.name.name == leaf_class)
                .is_some_and(|c| c.methods.iter().any(|m| m.selector.name() == selector));
        };

        let mut ordered: Vec<ecow::EcoString> = hierarchy
            .superclass_chain(leaf_class)
            .into_iter()
            .rev()
            .collect();
        ordered.push(ecow::EcoString::from(leaf_class));

        ordered.iter().any(|name| {
            if excluded_defaults.contains(&name.as_str()) {
                return false;
            }
            hierarchy
                .get_class(name.as_str())
                .is_some_and(|info| info.methods.iter().any(|m| m.selector == selector))
        })
    }

    /// ADR 0078: Collect typed-no-default state fields across the
    /// inheritance chain for post-initialize validation.
    ///
    /// Returns fields in parent-first order (ancestors first, then the leaf's
    /// own fields), annotated with the owning class name so diagnostics can
    /// point at the class that declared the field.
    ///
    /// When an ancestor's AST is not present in the current `Module`
    /// (cross-file parent), the function falls back to `ClassInfo` metadata
    /// loaded from that ancestor's `__beamtalk_meta/0` (see `state_types` /
    /// `state_has_default`). This closes the cross-file inherited-field
    /// validation gap.
    pub(in crate::core_erlang) fn inherited_typed_no_default_fields(
        &self,
        module: &Module,
        leaf_class: &str,
    ) -> Vec<InheritedTypedField> {
        let Some(hierarchy) = self.class_hierarchy.as_ref() else {
            // Fallback: AST only, leaf class fields.
            return module
                .classes
                .iter()
                .find(|c| c.name.name == leaf_class)
                .map(|c| {
                    c.state
                        .iter()
                        .filter(|s| {
                            beamtalk_core::semantic_analysis::requires_definite_assignment(
                                s,
                                &self.alias_registry,
                            )
                        })
                        .map(|s| InheritedTypedField {
                            owning_class: c.name.name.to_string(),
                            field_name: s.name.name.to_string(),
                            type_name: s.type_annotation.as_ref().map_or_else(
                                || "Unknown".to_string(),
                                |ta| ta.type_name().to_string(),
                            ),
                        })
                        .collect::<Vec<_>>()
                })
                .unwrap_or_default();
        };

        let mut ordered: Vec<ecow::EcoString> = hierarchy
            .superclass_chain(leaf_class)
            .into_iter()
            .rev()
            .collect();
        ordered.push(ecow::EcoString::from(leaf_class));

        let mut out = Vec::new();
        for name in ordered {
            if matches!(name.as_str(), "Actor" | "Object" | "ProtoObject") {
                continue;
            }
            if let Some(class) = module.classes.iter().find(|c| c.name.name == name) {
                for s in &class.state {
                    if beamtalk_core::semantic_analysis::requires_definite_assignment(
                        s,
                        &self.alias_registry,
                    ) {
                        out.push(InheritedTypedField {
                            owning_class: class.name.name.to_string(),
                            field_name: s.name.name.to_string(),
                            type_name: s.type_annotation.as_ref().map_or_else(
                                || "Unknown".to_string(),
                                |ta| ta.type_name().to_string(),
                            ),
                        });
                    }
                }
            } else if let Some(info) = hierarchy.get_class(&name) {
                // Cross-file ancestor — use ClassInfo metadata.
                // Preserve the declaration order by iterating `info.state`.
                for field_name in &info.state {
                    let Some(type_name) = info.state_types.get(field_name) else {
                        continue; // untyped field — not a typed-no-default
                    };
                    // `ClassHierarchy::state_field_has_default` conservatively
                    // treats a missing default-info entry (e.g., older BEAM
                    // artifact) as "has default" so we don't raise a spurious
                    // UninitializedStateError on stale metadata — see its doc
                    // comment for the shared degradation rule this and
                    // `TypeChecker::check_value_construction_definite_assignment`
                    // both rely on.
                    if hierarchy.state_field_has_default(&name, field_name) {
                        continue;
                    }
                    // ADR 0124 §1/B2: a `late` slot opts out of definite
                    // assignment exactly like the AST path's
                    // `requires_definite_assignment` (`SlotKind::Late` short-
                    // circuits before nilability is even considered) —
                    // mirrored here via `state_kinds` since a cross-file
                    // ancestor has no `StateDeclaration` to hand that
                    // predicate directly. Missing metadata (an older
                    // `__beamtalk_meta/0` predating `state_kinds`) degrades to
                    // `SlotKind::Eager`, matching `state_field_kind`'s own
                    // fallback.
                    let kind = info
                        .state_kinds
                        .get(field_name)
                        .copied()
                        .unwrap_or(SlotKind::Eager);
                    if kind == SlotKind::Late {
                        continue;
                    }
                    let type_name = type_name.to_string();
                    if beamtalk_core::semantic_analysis::is_nilable_type_name(
                        &type_name,
                        &self.alias_registry,
                    ) {
                        continue;
                    }
                    out.push(InheritedTypedField {
                        owning_class: info.name.to_string(),
                        field_name: field_name.to_string(),
                        type_name,
                    });
                }
            }
        }
        out
    }

    /// Generates the `handle_continue/2` callback.
    ///
    /// ADR 0078: Auto-chains `initialize` methods parent-first across
    /// the full inheritance hierarchy. For each ancestor class that defines its
    /// own `initialize`, we emit a `safe_dispatch` call, threading the state
    /// from one initializer to the next. After the chain completes, the
    /// post-initialize check validates that every typed-no-default
    /// field (from any class in the chain) was set.
    ///
    /// Dispatches run via `beamtalk_dispatch:lookup/5` with each class's own
    /// compiled module name so that dispatch starts *at* that class and walks
    /// upward — which finds the class's own `initialize` if present, bypassing
    /// any overriding subclass method (this is `super`-like semantics).
    ///
    /// This runs after `init/1` returns, so the `gen_server` message loop is
    /// active and self-sends do not deadlock. OTP guarantees no other messages
    /// are processed before `handle_continue`.
    ///
    /// # Generated Code (single-class — no user ancestors)
    ///
    /// ```erlang
    /// 'handle_continue'/2 = fun (Continue, State) ->
    ///     case Continue of
    ///         <'initialize'> when 'true' ->
    ///             <stash State> in
    ///             let _InitResult0 = call 'module':'safe_dispatch'('initialize', [], State) in
    ///             <restore State> in
    ///             case _InitResult0 of
    ///                 <{'reply', _InitRet0, InitNewState}> when 'true' ->
    ///                     <post-init check on InitNewState>
    ///                 ...errors...
    ///             end
    ///         <_> when 'true' -> {'noreply', State}
    ///     end
    /// ```
    ///
    /// # Generated Code (with ancestor chain `Grandparent -> Parent -> Child`)
    ///
    /// Grandparent runs first, its output state feeds Parent, whose output
    /// feeds Child, whose output is validated.
    #[allow(clippy::unnecessary_wraps)] // uniform Result<Document> codegen interface
    #[allow(clippy::too_many_lines)] // chain dispatch + error arms grow with hierarchy depth
    pub(in crate::core_erlang) fn generate_handle_continue(
        &self,
        module: &Module,
    ) -> Result<Document<'static>> {
        let module_name = self.module_name.clone();

        // Find typed-no-default fields (leaf + ancestors) for
        // post-initialize validation.
        let current_class = self.current_class(module);
        let leaf_class_name =
            current_class.map_or_else(|| module_name.clone(), |c| c.name.name.clone());
        let state_keys_owned = self.committed_state_keys(module);
        let state_keys = state_keys_owned.as_ref();
        let success_body = self.generate_post_initialize_check(
            current_class,
            module,
            state_keys,
            &leaf_class_name,
        );

        // ADR 0078: Walk the user-defined initialize chain (parent-first).
        // Each entry is the (class_name, compiled_module_name) for a class that
        // defines its own `initialize`. Root classes (Actor/Object/ProtoObject)
        // are filtered out since their `initialize` is a no-op default.
        let chain = current_class
            .map(|c| self.user_defined_initialize_chain(module, &c.name.name))
            .unwrap_or_default();

        // Body that runs after the dispatch chain: the post-initialize check.
        // If the chain is empty (e.g. only typed-no-default fields exist with no
        // initialize method), the post-init check runs directly against the
        // inbound `State` by binding it to `InitNewState`.
        let inner = if chain.is_empty() {
            bind(leaf::var("InitNewState"), leaf::var("State"), success_body)
        } else {
            // Build the dispatch chain from last to first so each step can nest
            // inside the previous one's success arm.
            let mut inner: Document<'static> = success_body;
            let n = chain.len();
            for (i, class) in chain.iter().enumerate().rev() {
                let idx = i.to_string();
                let is_last = i + 1 == n;
                let in_state_var = if i == 0 {
                    "State".to_string()
                } else {
                    format!("InitState{i}")
                };
                let result_var = format!("_InitResult{idx}");
                let reply_ret_var = format!("_InitRet{idx}");
                let next_state_var = if is_last {
                    "InitNewState".to_string()
                } else {
                    let next = i + 1;
                    format!("InitState{next}")
                };
                let err_state_var = format!("_InitErrState{idx}");
                let err_state_var2 = format!("_InitErrState{idx}b");
                let err_triple_type = format!("_InitType{idx}");
                let err_triple_reason = format!("_InitReason{idx}");
                let err_triple_stack = format!("_InitStack{idx}");
                let err_plain = format!("_InitError{idx}");
                let err_msg = format!("_InitErrMsg{idx}");
                let err_msg2 = format!("_InitErrMsg{idx}b");
                let err_class_name_var = format!("_InitErrClass{idx}");
                let err_class_name_var2 = format!("_InitErrClass{idx}b");

                let pdict = PdictVars::chain_step(&idx);

                let dispatch = form::call(
                    class.module_name.clone(),
                    "safe_dispatch",
                    [
                        leaf::atom("initialize"),
                        Document::Str("[]"),
                        leaf::var(in_state_var.clone()),
                    ],
                );
                let success_arm = arm(
                    [form::tuple([
                        leaf::atom("reply"),
                        leaf::var(reply_ret_var),
                        leaf::var(next_state_var),
                    ])],
                    inner,
                );
                // Destructure error triple to capture stacktrace
                let triple_arm = arm(
                    [form::tuple([
                        leaf::atom("error"),
                        form::tuple([
                            leaf::var(err_triple_type.clone()),
                            leaf::var(err_triple_reason.clone()),
                            leaf::var(err_triple_stack.clone()),
                        ]),
                        leaf::var(err_state_var.clone()),
                    ])],
                    log_and_stop_arm(LogAndStop {
                        msg_var: err_msg,
                        class_name_var: err_class_name_var,
                        module_name: &class.module_name,
                        format_safe_args: docvec![
                            form::tuple([
                                leaf::var(err_triple_type.clone()),
                                leaf::var(err_triple_reason.clone()),
                            ]),
                            ", ",
                            leaf::var(err_triple_stack.clone()),
                        ],
                        formatted_var: None,
                        reason: form::tuple([
                            leaf::var(err_triple_type.clone()),
                            leaf::var(err_triple_reason.clone()),
                        ]),
                        stacktrace: Some(leaf::var(err_triple_stack.clone())),
                        stop_reason: form::tuple([
                            leaf::var(err_triple_type),
                            leaf::var(err_triple_reason),
                            leaf::var(err_triple_stack),
                        ]),
                        stop_state: leaf::var(err_state_var),
                    }),
                );
                // Fallback: plain {error, Error, State} from dispatch (DNU, #beamtalk_error{}, etc.)
                let plain_arm = arm(
                    [form::tuple([
                        leaf::atom("error"),
                        leaf::var(err_plain.clone()),
                        leaf::var(err_state_var2.clone()),
                    ])],
                    log_and_stop_arm(LogAndStop {
                        msg_var: err_msg2,
                        class_name_var: err_class_name_var2,
                        module_name: &class.module_name,
                        format_safe_args: leaf::var(err_plain.clone()),
                        formatted_var: None,
                        reason: leaf::var(err_plain.clone()),
                        stacktrace: None,
                        stop_reason: leaf::var(err_plain),
                        stop_state: leaf::var(err_state_var2),
                    }),
                );

                inner = Self::pdict_stash(
                    &pdict,
                    &in_state_var,
                    bind(
                        leaf::var(result_var.clone()),
                        dispatch,
                        Self::pdict_restore(
                            &pdict,
                            case_block(leaf::var(result_var), [success_arm, triple_arm, plain_arm]),
                        ),
                    ),
                );
            }
            inner
        };

        let wrapped = Self::pending_actions_wrap(case_block(
            leaf::var("Continue"),
            [
                // The auto-chained dispatch body. Each class's initialize is
                // bracketed by its own pdict stash/restore pair to preserve
                // re-entrant self-send semantics.
                arm([leaf::atom("initialize")], inner),
                // BT-3968: one queued `self continueWith:`
                // step. The rest of the queue is seeded back
                // first (FIFO: a request made by this step
                // runs after them), then the step dispatches
                // through the very same case as a
                // fire-and-forget cast — pdict stash/restore,
                // strip_local_temps, notify_state_change and
                // the shared no-reply error arms, so a raising
                // step crashes the actor (ADR 0134) — by binding
                // CastSelector / CastArgs in the pattern.
                arm(
                    [form::tuple([
                        leaf::atom(sentinel_tags::CONTINUE),
                        docvec!["[{CastSelector, CastArgs}|ContRest]"],
                    ])],
                    bind(
                        leaf::var("_ContSeeded"),
                        form::call(
                            "beamtalk_actor",
                            "seed_pending_continuations",
                            [leaf::var("ContRest")],
                        ),
                        Self::cast_dispatch_case(&module_name, state_keys),
                    ),
                ),
                arm_inline([leaf::var("_")], noreply(leaf::var("State"))),
            ],
        ));

        let doc = docvec![
            form::fun_def(
                "handle_continue",
                [leaf::var("Continue"), leaf::var("State")],
                docvec![
                    // BT-3992: once the `initialize` step (with its pending
                    // actions applied) has its final result, tell the starter
                    // startup is over — before OTP runs any continuation
                    // `initialize` requested. A no-op for every other step.
                    //
                    // `in` sits on its own line here, a layout no
                    // `form::let_in` produces.
                    "let _ContinueResult =",
                    nest(INDENT, docvec![line(), wrapped]),
                    line(),
                    "in ",
                    form::call(
                        "beamtalk_actor",
                        "ack_initialized",
                        [leaf::var("Continue"), leaf::var("_ContinueResult")],
                    ),
                ],
                Layout::Block,
            ),
            "\n",
            "\n",
        ];
        Ok(doc)
    }

    /// BT-3967 / BT-3968: wraps a `gen_server` callback body so its return
    /// value passes through `beamtalk_actor:apply_pending_actions/1`.
    ///
    /// `self stopWith: reason` inside a method cannot call
    /// `gen_server:stop/3` on its own process (`calling_self`), and
    /// `self continueWith: #sel` must run only after the current callback
    /// returns, so the runtime records both requests in the process
    /// dictionary instead; this wrapper is where they take effect — after
    /// the current dispatch has returned. A pending stop turns
    /// `{reply, R, S}` / `{noreply, S}` into `{stop, Reason, R, S}` /
    /// `{stop, Reason, S}`; otherwise pending continuations add a
    /// `{continue, {'$bt_continue', Queue}}` action, handled by the
    /// `'$bt_continue'` clause of the generated `handle_continue/2`. Any other
    /// return value, and every callback with no pending request, passes
    /// through unchanged. `clear_pending_actions/0` runs first, so only a
    /// request made by this callback's own dispatch can take effect.
    fn pending_actions_wrap(body: Document<'static>) -> Document<'static> {
        bind(
            leaf::var("_PendingActionsCleared"),
            form::call("beamtalk_actor", "clear_pending_actions", []),
            // `in` sits on its own line here, a layout no `form::let_in`
            // produces.
            docvec![
                "let _CallbackResult =",
                nest(INDENT, docvec![line(), body]),
                line(),
                "in ",
                form::call(
                    "beamtalk_actor",
                    "apply_pending_actions",
                    [leaf::var("_CallbackResult")],
                ),
            ],
        )
    }

    /// BT-3968: `handle_call/3`'s dispatch behind the mailbox-boundary guard
    /// (`beamtalk_actor:mailbox_continuation_check/2`). `self continueWith:`
    /// is a direct in-process send and never arrives through the mailbox, so
    /// a call message requesting a continuation — directly or via a
    /// `perform:` — came from another process and is answered with
    /// `invalid_continuation` instead of being dispatched inside the actor,
    /// where the runtime's `self()` check could not tell.
    fn mailbox_call_guard(
        module_name: &ecow::EcoString,
        state_keys: Option<&CommittedStateKeys>,
    ) -> Document<'static> {
        Self::mailbox_guard(
            "Selector",
            "Args",
            "ContErr",
            Self::handle_call_dispatch_case(module_name, state_keys),
            form::tuple([
                leaf::atom("reply"),
                form::tuple([leaf::atom("error"), leaf::var("ContErr")]),
                leaf::var("State"),
            ]),
        )
    }

    /// BT-3968: `handle_cast/2`'s fire-and-forget dispatch behind the same
    /// guard as [`Self::mailbox_call_guard`]; a rejected cast (logged by the
    /// runtime check) is dropped.
    fn mailbox_cast_guard(
        module_name: &ecow::EcoString,
        state_keys: Option<&CommittedStateKeys>,
    ) -> Document<'static> {
        Self::mailbox_guard(
            "CastSelector",
            "CastArgs",
            "_ContErr",
            Self::cast_dispatch_case(module_name, state_keys),
            noreply(leaf::var("State")),
        )
    }

    fn mailbox_guard(
        selector_var: &'static str,
        args_var: &'static str,
        error_var: &'static str,
        accepted: Document<'static>,
        rejected: Document<'static>,
    ) -> Document<'static> {
        case_block(
            form::call(
                "beamtalk_actor",
                "mailbox_continuation_check",
                [leaf::var(selector_var), leaf::var(args_var)],
            ),
            [
                arm([leaf::atom("ok")], accepted),
                arm(
                    [form::tuple([leaf::atom("error"), leaf::var(error_var)])],
                    rejected,
                ),
            ],
        )
    }

    /// BT-3968: the `let` binding that drops the continuation queue in
    /// `handle_call/3`'s dispatch-error arm — the follow-up steps a failed
    /// dispatch requested are void. (The no-reply error arms crash the actor
    /// instead, which drops the whole queue; ADR 0134.)
    fn discard_continuations_let(body: Document<'static>) -> Document<'static> {
        bind(
            leaf::var("_ContDiscarded"),
            form::call("beamtalk_actor", "discard_pending_continuations", []),
            body,
        )
    }

    /// `let <old> = <install state_var> in body`: stashes `state_var` for
    /// re-entrant self-sends around `body`, which must close the bracket with
    /// [`Self::pdict_restore`] using the same `vars`.
    fn pdict_stash(
        vars: &PdictVars,
        state_var: &str,
        body: Document<'static>,
    ) -> Document<'static> {
        bind(
            leaf::var(vars.old.clone()),
            actor_state_call(
                actor_state_helper::INSTALL,
                [leaf::var(state_var.to_string())],
            ),
            body,
        )
    }

    /// Wraps `body` in the uninstall of the stashed state and the erase of the
    /// call stack. `body` follows the dispatch result binding (for example
    /// `let _DispatchResult = ... in`).
    fn pdict_restore(vars: &PdictVars, body: Document<'static>) -> Document<'static> {
        bind(
            leaf::var(vars.restore.clone()),
            actor_state_call(actor_state_helper::UNINSTALL, [leaf::var(vars.old.clone())]),
            bind(
                leaf::var(vars.clear.clone()),
                form::call("erlang", "erase", [leaf::atom(sentinel_tags::CALL_STACK)]),
                body,
            ),
        )
    }

    /// The inner `case safe_dispatch ... end` for a fire-and-forget cast (and
    /// a `continueWith:` step, which reuses it). A successful dispatch commits
    /// its new state and discards the result; a raising one crashes the actor
    /// through the shared no-reply error arms (ADR 0134, BT-3958).
    fn cast_dispatch_case(
        module_name: &ecow::EcoString,
        state_keys: Option<&CommittedStateKeys>,
    ) -> Document<'static> {
        let vars = PdictVars::reentrant();
        // Stash State for re-entrant self-sends
        Self::pdict_stash(
            &vars,
            "State",
            // safe_dispatch turns a raise into an {error, ...} value; the
            // error arms below re-raise it after the pdict is restored
            bind(
                leaf::var("_CastDispatchResult"),
                form::call(
                    module_name.clone(),
                    "safe_dispatch",
                    [
                        leaf::var("CastSelector"),
                        leaf::var("CastArgs"),
                        leaf::var("State"),
                    ],
                ),
                // Restore pdict
                Self::pdict_restore(
                    &vars,
                    case_block(leaf::var("_CastDispatchResult"), {
                        let [triple_arm, plain_arm] = noreply_error_arms("Cast");
                        [
                            arm(
                                [form::tuple([
                                    leaf::atom("reply"),
                                    leaf::var("_CastResult"),
                                    leaf::var("CastNewState"),
                                ])],
                                // strip codegen-internal `__local__` threading
                                // temporaries before persist + notify (see handle_call).
                                clean_state_let(
                                    "CleanCastNewState",
                                    "CastNewState",
                                    state_keys,
                                    // same per-object change push as handle_call, for
                                    // a state write committed via a fire-and-forget cast.
                                    bind(
                                        leaf::var("_CastStateChanged"),
                                        form::call(
                                            "beamtalk_actor",
                                            "notify_state_change",
                                            [leaf::var("State"), leaf::var("CleanCastNewState")],
                                        ),
                                        noreply(leaf::var("CleanCastNewState")),
                                    ),
                                ),
                            ),
                            // ADR 0134: re-raise, crashing the actor — shared with handle_info
                            triple_arm,
                            plain_arm,
                        ]
                    }),
                ),
            ),
        )
    }

    /// Generates the `handle_cast/2` callback for fire-and-forget (`!`) sends,
    /// `{cast, Selector, Args}` from `beamtalk_actor:cast_send/3`. The result
    /// is discarded; an error raised by the method crashes the actor (ADR
    /// 0134, BT-3958) through the error arms shared with `handle_info/2` —
    /// there is no caller to hand it to.
    ///
    /// ADR 0069 Phase 2b: Matches 4-tuple messages with `PropCtx`
    /// for both fire-and-forget casts and async sends. Falls back to 3-tuple
    /// for backward compatibility.
    ///
    /// ADR 0126 §5.1 (BT-3613): a leading clause recognises a
    /// `'$beamtalk_wire'`-tagged remote fire-and-forget cast by shape
    /// (6-tuple headed by that atom, `cast` kind) and hands it to
    /// `beamtalk_actor:decode_wire_cast/1` — the single place the tag's
    /// decode/version-check logic lives, shared with
    /// `beamtalk_actor.erl`'s own `handle_cast/2` (AGENTS.md "No duplicate
    /// implementations"). A decoded envelope re-dispatches, by a recursive
    /// self-call, through the `{'cast', ...}` clause below exactly as if it
    /// had arrived locally; a decode/version failure has already been
    /// logged and dropped by the shared helper. The 7-tuple `async`-kind
    /// wire shape is deliberately not matched here: no compiled `.bt`
    /// construct reaches the async-with-future protocol at all (see
    /// `decode_wire_cast/1`'s own doc), so an async-tagged wire cast falls
    /// through to the ordinary catch-all below exactly like an
    /// un-wire-tagged one already does today.
    #[allow(clippy::unnecessary_wraps)] // uniform Result<Document> codegen interface
    pub(in crate::core_erlang) fn generate_handle_cast(
        &mut self,
        module: &Module,
    ) -> Result<Document<'static>> {
        let module_name = self.module_name.clone();
        let state_keys_owned = self.committed_state_keys(module);
        let state_keys = state_keys_owned.as_ref();
        let doc = docvec![
            form::fun_def(
                "handle_cast",
                [leaf::var("Msg"), leaf::var("State")],
                Self::pending_actions_wrap(case_block(
                    leaf::var("Msg"),
                    [
                        // Wire-tagged remote fire-and-forget cast (ADR 0126
                        // §5.1) — decode via the shared helper, then
                        // redispatch or drop.
                        arm(
                            [form::tuple([
                                leaf::atom(sentinel_tags::WIRE),
                                leaf::var("_WireVersion"),
                                leaf::atom("cast"),
                                leaf::var("_WireSelector"),
                                leaf::var("_WireArgs"),
                                leaf::var("_WirePropCtx"),
                            ])],
                            case_block(
                                form::call(
                                    "beamtalk_actor",
                                    "decode_wire_cast",
                                    [leaf::var("Msg")],
                                ),
                                [
                                    arm(
                                        [form::tuple([
                                            leaf::atom("redispatch"),
                                            leaf::var("WireMsg2"),
                                        ])],
                                        form::call(
                                            module_name.clone(),
                                            "handle_cast",
                                            [leaf::var("WireMsg2"), leaf::var("State")],
                                        ),
                                    ),
                                    arm([leaf::atom("noreply")], noreply(leaf::var("State"))),
                                ],
                            ),
                        ),
                        // Fire-and-forget cast with propagated context
                        arm(
                            [form::tuple([
                                leaf::atom("cast"),
                                leaf::var("CastSelector"),
                                leaf::var("CastArgs"),
                                leaf::var("CastPropCtx"),
                            ])],
                            bind(
                                leaf::var("_CastCtxOk"),
                                form::call(
                                    "beamtalk_actor",
                                    "restore_propagated_ctx",
                                    [leaf::var("CastPropCtx")],
                                ),
                                Self::mailbox_cast_guard(&module_name, state_keys),
                            ),
                        ),
                        // Fire-and-forget cast without context (backward compat)
                        arm(
                            [form::tuple([
                                leaf::atom("cast"),
                                leaf::var("CastSelector"),
                                leaf::var("CastArgs"),
                            ])],
                            Self::mailbox_cast_guard(&module_name, state_keys),
                        ),
                        arm_inline([leaf::var("_")], noreply(leaf::var("State"))),
                    ],
                )),
                Layout::Block,
            ),
            "\n",
            "\n",
        ];
        Ok(doc)
    }

    /// Generates the `handle_call/3` callback for sync message sends.
    ///
    /// Uses `safe_dispatch/3` for error isolation and
    /// returns `{ok, Result}` or `{error, Error}` tuples.
    ///
    /// ADR 0069 Phase 2b: Matches 3-tuple `{Selector, Args, PropCtx}`
    /// to restore propagated context (`OTel` trace context) before dispatch.
    /// Falls back to 2-tuple `{Selector, Args}` for backward compatibility.
    ///
    /// ADR 0126 §5.1 (BT-3613): a leading clause recognises a
    /// `'$beamtalk_wire'`-tagged remote call by shape (6-tuple headed by
    /// that atom) and hands it to `beamtalk_actor:decode_wire_call/1` —
    /// the single place the tag's decode/version-check logic lives
    /// (`beamtalk_actor.erl`'s own `handle_call/3` calls the very same
    /// function for its hand-written `__methods__` path, AGENTS.md "No
    /// duplicate implementations"). A decoded envelope re-dispatches, by a
    /// recursive self-call, through the 3-tuple clause below exactly as if
    /// it had arrived locally; a decode/version failure's ready-made reply
    /// is returned with `State` untouched, without ever reaching
    /// `safe_dispatch/3`.
    #[allow(clippy::unnecessary_wraps)] // uniform Result<Document> codegen interface
    pub(in crate::core_erlang) fn generate_handle_call(
        &mut self,
        module: &Module,
    ) -> Result<Document<'static>> {
        let module_name = self.module_name.clone();
        let dispatch_case =
            Self::mailbox_call_guard(&module_name, self.committed_state_keys(module).as_ref());
        let doc = docvec![
            form::fun_def(
                "handle_call",
                [leaf::var("Msg"), leaf::var("From"), leaf::var("State")],
                Self::pending_actions_wrap(case_block(
                    leaf::var("Msg"),
                    [
                        // Wire-tagged remote call (ADR 0126 §5.1) — decode via
                        // the shared helper, then redispatch or reply directly.
                        arm(
                            [form::tuple([
                                leaf::atom(sentinel_tags::WIRE),
                                leaf::var("_WireVersion"),
                                leaf::atom("call"),
                                leaf::var("_WireSelector"),
                                leaf::var("_WireArgs"),
                                leaf::var("_WirePropCtx"),
                            ])],
                            case_block(
                                form::call(
                                    "beamtalk_actor",
                                    "decode_wire_call",
                                    [leaf::var("Msg")],
                                ),
                                [
                                    arm(
                                        [form::tuple([
                                            leaf::atom("redispatch"),
                                            leaf::var("WireMsg2"),
                                        ])],
                                        form::call(
                                            module_name.clone(),
                                            "handle_call",
                                            [
                                                leaf::var("WireMsg2"),
                                                leaf::var("From"),
                                                leaf::var("State"),
                                            ],
                                        ),
                                    ),
                                    arm(
                                        [form::tuple([
                                            leaf::atom("reply"),
                                            leaf::var("WireReplyVal"),
                                        ])],
                                        form::tuple([
                                            leaf::atom("reply"),
                                            leaf::var("WireReplyVal"),
                                            leaf::var("State"),
                                        ]),
                                    ),
                                ],
                            ),
                        ),
                        // 3-tuple with propagated context — restore before dispatch
                        arm(
                            [form::tuple([
                                leaf::var("Selector"),
                                leaf::var("Args"),
                                leaf::var("PropCtx"),
                            ])],
                            bind(
                                leaf::var("_CtxOk"),
                                form::call(
                                    "beamtalk_actor",
                                    "restore_propagated_ctx",
                                    [leaf::var("PropCtx")],
                                ),
                                dispatch_case.clone(),
                            ),
                        ),
                        // Backward compat: 2-tuple without context
                        arm(
                            [form::tuple([leaf::var("Selector"), leaf::var("Args")])],
                            dispatch_case,
                        ),
                    ],
                )),
                Layout::Block,
            ),
            "\n",
            "\n",
        ];
        Ok(doc)
    }

    /// BT-4090: the success arm's reply. A deferred call (`self deferReply`)
    /// is answered `{noreply, _}` *before* encoding, so the method's ignored
    /// return value is never encoded; otherwise the reply is wire-encoded and
    /// tagged exactly as described on [`Self::handle_call_dispatch_case`].
    fn success_reply_case() -> Document<'static> {
        case_block(
            leaf::var("_CallDeferred"),
            [
                arm([leaf::atom("true")], noreply(leaf::var("CleanNewState"))),
                arm(
                    [leaf::var("_NotDeferred")],
                    case_block(
                        form::call(
                            "beamtalk_actor",
                            "encode_reply_for_tagged",
                            [
                                leaf::var("From"),
                                leaf::var("Selector"),
                                leaf::var("Result"),
                            ],
                        ),
                        [
                            arm(
                                [form::tuple([leaf::atom("error"), leaf::var("EncErr")])],
                                form::tuple([
                                    leaf::atom("reply"),
                                    form::tuple([leaf::atom("error"), leaf::var("EncErr")]),
                                    leaf::var("CleanNewState"),
                                ]),
                            ),
                            arm(
                                [form::tuple([leaf::atom("ok"), leaf::var("EncodedResult")])],
                                form::tuple([
                                    leaf::atom("reply"),
                                    form::tuple([leaf::atom("ok"), leaf::var("EncodedResult")]),
                                    leaf::var("CleanNewState"),
                                ]),
                            ),
                        ],
                    ),
                ),
            ],
        )
    }

    /// Generates the inner `case safe_dispatch ... end` block for `handle_call`.
    /// Shared between 3-tuple (with `PropCtx`) and 2-tuple (backward compat) patterns.
    ///
    /// ADR 0126 §5.1/§5.5 (BT-3613): both arms route their payload through
    /// `beamtalk_actor` helpers shared with the hand-written dispatch path
    /// rather than re-deriving the same logic in codegen (AGENTS.md "No
    /// duplicate implementations") — `encode_reply_for/3` wire-encodes the
    /// reply when `From` is on another node (a no-op for a local caller,
    /// so this is byte-identical to before for every existing local-call
    /// test), and the error arm's `maybe_reclassify_compiled_dispatch_error/2`
    /// reclassifies a stale-block badfun/undef as `remote_code_mismatch`
    /// before that same encode step, leaving every other error shape
    /// untouched.
    ///
    /// The success arm branches on `encode_reply_for/3`'s own result rather
    /// than unconditionally wrapping it as `{'ok', EncodedResult}`: a
    /// wire-encode failure (e.g. the method returned a `HandleScoped` value
    /// to a remote caller) makes `encode_reply_for/3` return `{'error',
    /// EncErr}` instead of the encoded payload, per its own doc ("An encode
    /// failure never crashes the callee: it replaces the reply with
    /// `{error, #beamtalk_error{}}` instead"). Wrapping that in `{'ok', ...}`
    /// would hand the caller `{'ok', {'error', EncErr}}` — which decodes and
    /// unwraps back to the plain tuple `{'error', EncErr}` as a
    /// *successful* method return value instead of raising, silently
    /// breaking ADR 0126 §5.1's documented `not_serialisable` contract.
    ///
    /// Branching on `encode_reply_for/3`'s own (untagged) result is
    /// unsound here, though: that function is a no-op passthrough for a
    /// local caller, so a method that legitimately returns a raw `Tuple`
    /// shaped like `{error, X}` (a normal, first-class `BeamTalk` value —
    /// `stdlib/src/tuple.bt`) would be indistinguishable from a genuine
    /// encode failure by shape alone. `encode_reply_for_tagged/3` exists
    /// precisely for this call site: it always outer-tags its result
    /// `{ok, _}`/`{error, _}` — `ok` for both the remote-encoded-success
    /// case and the local passthrough case, `error` only for a genuine
    /// remote encode failure — so matching the outer tag never depends on
    /// what the payload itself looks like. (The hand-written
    /// `beamtalk_actor:handle_call/3` keeps using the untagged
    /// `encode_reply_for/3` unchanged — it never branches on the result,
    /// it builds the reply's own outer shape itself at each call site, so
    /// it was never at risk of this ambiguity.)
    fn handle_call_dispatch_case(
        module_name: &ecow::EcoString,
        state_keys: Option<&CommittedStateKeys>,
    ) -> Document<'static> {
        let vars = PdictVars::reentrant();
        // Stash State for re-entrant self-sends
        Self::pdict_stash(
            &vars,
            "State",
            // BT-4090: expose the caller's `From` to `self deferReply`
            bind(
                leaf::var("_CallFromInstalled"),
                form::call("beamtalk_actor", "install_call_from", [leaf::var("From")]),
                // Use safe_dispatch for error isolation
                bind(
                    leaf::var("_DispatchResult"),
                    form::call(
                        module_name.clone(),
                        "safe_dispatch",
                        [leaf::var("Selector"), leaf::var("Args"), leaf::var("State")],
                    ),
                    // Restore pdict
                    Self::pdict_restore(
                        &vars,
                        // BT-4090: remove the `From` cell; `true` when the method deferred
                        // its reply. Only the success arm acts on it — error arms reply
                        // the error, so a raise after deferring never hangs the caller.
                        bind(
                            leaf::var("_CallDeferred"),
                            form::call("beamtalk_actor", "uninstall_call_from", []),
                            case_block(
                                leaf::var("_DispatchResult"),
                                [Self::call_success_arm(state_keys), Self::call_error_arm()],
                            ),
                        ),
                    ),
                ),
            ),
        )
    }

    /// `handle_call/3`'s success arm: `<{'reply', Result, NewState}>`.
    fn call_success_arm(state_keys: Option<&CommittedStateKeys>) -> Document<'static> {
        // Success case: return {ok, Result}
        arm(
            [form::tuple([
                leaf::atom("reply"),
                leaf::var("Result"),
                leaf::var("NewState"),
            ])],
            // strip codegen-internal `__local__` threading
            // temporaries before persist + notify, so an outer local
            // threaded through a control-flow desugar never leaks into
            // the committed actor state or a watch notification.
            clean_state_let(
                "CleanNewState",
                "NewState",
                state_keys,
                // notify the per-object change substrate so a
                // watched compiled actor's committed state write pushes
                // {object_changed, …} to the live Inspector — the runtime
                // beamtalk_actor path does this via log_dispatch_complete.
                bind(
                    leaf::var("_StateChanged"),
                    form::call(
                        "beamtalk_actor",
                        "notify_state_change",
                        [leaf::var("State"), leaf::var("CleanNewState")],
                    ),
                    // wire-encode the reply when the caller is remote
                    // (no-op locally — see `handle_call_dispatch_case`'s doc), then
                    // branch on the outer {ok,_}/{error,_} tag
                    // encode_reply_for_tagged/3 itself always adds —
                    // never on the payload's own shape.
                    //
                    // BT-4090: a deferred reply (`self deferReply`) is
                    // answered `{noreply, _}` *before* encoding — the
                    // method's ignored return value must not be encoded.
                    Self::success_reply_case(),
                ),
            ),
        )
    }

    /// `handle_call/3`'s error arm, `<{'error', Error, ErrState}>`.
    //
    // Error case: reclassify a stale-block badfun/undef, then
    // pass the (possibly reclassified) error opaquely to the
    // caller, wire-encoded when remote. Same branch-not-wrap
    // reasoning as the success arm: an encode failure
    // here would otherwise double-wrap as
    // {'error', {'error', EncErr}} instead of the flat
    // {'error', EncErr} shape decode_call_result expects.
    // Unlike the success arm, this one stays on the
    // untagged encode_reply_for/3, not the _tagged variant
    // — both of its branches already converge on the same
    // {'reply', {'error', _}, ErrState} outer shape, so
    // there's no {'ok', _} vs {'error', _} choice for a
    // payload shape collision to corrupt.
    fn call_error_arm() -> Document<'static> {
        let error_reply = |error: Document<'static>| {
            form::tuple([
                leaf::atom("reply"),
                form::tuple([leaf::atom("error"), error]),
                leaf::var("ErrState"),
            ])
        };
        arm(
            [form::tuple([
                leaf::atom("error"),
                leaf::var("Error"),
                leaf::var("ErrState"),
            ])],
            Self::discard_continuations_let(
                // BT-4850: a caught exception's raw trace is built here,
                // in the actor that caught it, so the caller's
                // `ensure_wrapped` keeps the raise-site frames.
                bind(
                    leaf::var("BuiltError"),
                    build_caught_triple_doc(leaf::var("Error"), "Reply"),
                    bind(
                        leaf::var("ClassifiedError"),
                        form::call(
                            "beamtalk_actor",
                            "maybe_reclassify_compiled_dispatch_error",
                            [leaf::var("Args"), leaf::var("BuiltError")],
                        ),
                        case_block(
                            form::call(
                                "beamtalk_actor",
                                "encode_reply_for",
                                [
                                    leaf::var("From"),
                                    leaf::var("Selector"),
                                    leaf::var("ClassifiedError"),
                                ],
                            ),
                            [
                                arm(
                                    [form::tuple([leaf::atom("error"), leaf::var("EncErr")])],
                                    error_reply(leaf::var("EncErr")),
                                ),
                                arm(
                                    [leaf::var("EncodedError")],
                                    error_reply(leaf::var("EncodedError")),
                                ),
                            ],
                        ),
                    ),
                ),
            ),
        )
    }

    /// Generates the `handle_info/2` callback (ADR 0065).
    ///
    /// BT-3596: In both branches, a non-parent `'EXIT'` message is
    /// intercepted first via `beamtalk_actor:handle_linked_exit/2`, before
    /// the Server-subclass `handleInfo:` dispatch and before the default
    /// ignore-all delegate — mirroring the crash-propagation semantics an
    /// *untrapped* link already has (this only ever fires for an actor that
    /// traps exits; see `generate_init_function`'s `trap_exit` gating). The
    /// `gen_server` loop's own parent-EXIT handling already calls
    /// `terminate/2` and exits before `handle_info/2` runs at all, so any
    /// `'EXIT'` reaching here is from some other link (e.g. a child the
    /// actor itself spawned and linked) — without this, that crash would be
    /// silently swallowed by the ignore-all default.
    ///
    /// For **Server subclasses**, dispatches to the user-defined `handleInfo:`
    /// method. If `handleInfo:` raises, the error is re-raised and the server
    /// crashes (ADR 0134, BT-3958): `gen_server` calls `terminate/2` with the
    /// error as its reason, emits a crash report, and the supervisor applies
    /// its restart policy. The error arms are shared with `handle_cast/2`
    /// (`noreply_error_arms`).
    ///
    /// For **plain Actor subclasses**, generates the default ignore-all stub that
    /// delegates to `beamtalk_actor:handle_info/2` — which itself checks for a
    /// linked `'EXIT'` first (see that function's doc comment).
    ///
    /// # Generated Code (Server subclass)
    ///
    /// ```erlang
    /// 'handle_info'/2 = fun (Msg, State) ->
    ///     case call 'beamtalk_actor':'handle_linked_exit'(Msg, State) of
    ///         <'pass'> when 'true' ->
    ///             case call 'Module':'safe_dispatch'('handleInfo:', [Msg], State) of
    ///                 <{'reply', _Result, NewState}> when 'true' -> {'noreply', NewState}
    ///                 <{'error', {InfoType, InfoReason, InfoStacktrace}, _InfoState}> when 'true' ->
    ///                     let InfoBuiltStack = primop 'build_stacktrace'(InfoStacktrace) in
    ///                     let InfoException = call 'beamtalk_exception_handler':'ensure_wrapped'(InfoType, InfoReason, InfoBuiltStack) in
    ///                     primop 'raw_raise'('error', InfoException, InfoStacktrace)
    ///                 <{'error', InfoError, _InfoState2}> when 'true' ->
    ///                     call 'beamtalk_exception_handler':'reraise'(InfoError)
    ///                 <_Other> when 'true' -> {'noreply', State}
    ///             end
    ///         <_ExitResult> when 'true' -> _ExitResult
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
    pub(in crate::core_erlang) fn generate_handle_info(
        &self,
        module: &Module,
    ) -> Result<Document<'static>> {
        if self.is_server_subclass {
            // BT-4052: baked flag - true when the class (or an ancestor other
            // than `Server`) defines `onLinkedExit:reason:`.
            let has_hook = self.current_class(module).is_some_and(|c| {
                self.class_or_ancestor_defines_linked_exit_hook(module, &c.name.name)
            });
            let state_keys_owned = self.committed_state_keys(module);
            let state_keys = state_keys_owned.as_ref();
            let dispatch_body = self.info_dispatch_doc("handleInfo:", "[Msg]", state_keys);
            let hook_arm = has_hook.then(|| {
                arm(
                    [form::tuple([
                        leaf::atom("hook"),
                        leaf::var("HookPid"),
                        leaf::var("HookReason"),
                    ])],
                    // The leading `line()` is the blank line this arm has
                    // always had (see `linked_exit_intercept_doc`).
                    docvec![
                        line(),
                        self.info_dispatch_doc(
                            "onLinkedExit:reason:",
                            "[HookPid, HookReason]",
                            state_keys,
                        ),
                    ],
                )
            });
            let doc = docvec![
                form::fun_def(
                    "handle_info",
                    [leaf::var("Msg"), leaf::var("State")],
                    Self::pending_actions_wrap(Self::linked_exit_intercept_doc(
                        dispatch_body,
                        hook_arm,
                        has_hook,
                    )),
                    Layout::Block,
                ),
                "\n",
                "\n",
            ];
            Ok(doc)
        } else {
            let doc = docvec![
                form::fun_def(
                    "handle_info",
                    [leaf::var("Msg"), leaf::var("State")],
                    form::call(
                        "beamtalk_actor",
                        "handle_info",
                        [leaf::var("Msg"), leaf::var("State")],
                    ),
                    Layout::Block,
                ),
                "\n",
                "\n",
            ];
            Ok(doc)
        }
    }

    /// The `safe_dispatch` + state-commit + error-arms block shared by the
    /// `handleInfo:` dispatch and (BT-4052) the `onLinkedExit:reason:` hook
    /// dispatch in `handle_info/2`. `args` is a Core Erlang list literal.
    fn info_dispatch_doc(
        &self,
        selector: &str,
        args: &'static str,
        state_keys: Option<&CommittedStateKeys>,
    ) -> Document<'static> {
        let vars = PdictVars::reentrant();
        // Stash State for re-entrant self-sends
        Self::pdict_stash(
            &vars,
            "State",
            bind(
                leaf::var("_InfoDispatchResult"),
                form::call(
                    self.module_name.clone(),
                    "safe_dispatch",
                    [
                        leaf::atom(selector.to_string()),
                        Document::Str(args),
                        leaf::var("State"),
                    ],
                ),
                Self::pdict_restore(&vars, {
                    let [triple_arm, plain_arm] = noreply_error_arms("Info");
                    case_block(
                        leaf::var("_InfoDispatchResult"),
                        [
                            arm(
                                [form::tuple([
                                    leaf::atom("reply"),
                                    leaf::var("_Result"),
                                    leaf::var("NewState"),
                                ])],
                                // handle_info is an outermost state-commit
                                // boundary too - a Server `handleInfo:` that threads an
                                // outer local through a control-flow desugar must not
                                // persist `__local__` temps into the committed state.
                                clean_state_let(
                                    "CleanInfoNewState",
                                    "NewState",
                                    state_keys,
                                    noreply(leaf::var("CleanInfoNewState")),
                                ),
                            ),
                            // ADR 0134: re-raise, crashing the actor - shared with handle_cast
                            triple_arm,
                            plain_arm,
                            arm_inline([leaf::var("_Other")], noreply(leaf::var("State"))),
                        ],
                    )
                }),
            ),
        )
    }

    /// BT-3596: Wraps a `handle_info/2` body in a `beamtalk_actor:handle_linked_exit/3`
    /// interception `case`. That runtime helper only ever produces a non-`'pass'`
    /// result for a trapping actor's non-parent `'EXIT'` message (see its own doc
    /// comment) - for every other message, and for any actor not trapping exits at
    /// all, it returns `'pass'` and `inner_body` runs exactly as before.
    ///
    /// BT-4052: the third argument is the baked hook flag; when true the helper
    /// returns `{'hook', Pid, Reason}` for every non-parent `'EXIT'`, which
    /// `hook_arm` dispatches to `onLinkedExit:reason:`.
    fn linked_exit_intercept_doc(
        inner_body: Document<'static>,
        hook_arm: Option<Document<'static>>,
        has_hook: bool,
    ) -> Document<'static> {
        // The leading `line()` in both arm bodies is the blank line the
        // generated `handle_info/2` has always had after each arm head
        // (the body, which starts with its own line break, sits under the
        // arm's); kept so the generated Core Erlang stays byte-identical.
        let clauses = std::iter::once(arm([leaf::atom("pass")], docvec![line(), inner_body]))
            .chain(hook_arm)
            .chain(std::iter::once(arm_inline(
                [leaf::var("_ExitResult")],
                leaf::var("_ExitResult"),
            )));
        case_block(
            form::call(
                "beamtalk_actor",
                "handle_linked_exit",
                [
                    leaf::var("Msg"),
                    leaf::var("State"),
                    leaf::atom(if has_hook { "true" } else { "false" }.to_string()),
                ],
            ),
            clauses,
        )
    }

    /// Whether `class`'s superclass is itself a compiled actor module whose
    /// generated callbacks (`init/1`'s inherited state, BT-4567's
    /// `'__beamtalk_secrets'/0`) this class chains to: any superclass other
    /// than the `Actor`/`Object` roots.
    fn has_parent_actor_module(class: &ClassDefinition) -> bool {
        class.superclass.as_ref().is_some_and(|s| {
            !s.name.eq_ignore_ascii_case("Actor") && !s.name.eq_ignore_ascii_case("Object")
        })
    }

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
    #[allow(clippy::unnecessary_wraps)] // uniform Result<Document> codegen interface
    pub(in crate::core_erlang) fn generate_code_change(&self) -> Result<Document<'static>> {
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
        Ok(doc)
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
    #[allow(clippy::unnecessary_wraps)] // uniform Result<Document> codegen interface
    pub(in crate::core_erlang) fn generate_terminate(
        &mut self,
        module: &Module,
    ) -> Result<Document<'static>> {
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
                Self::lifecycle_telemetry_let(
                    "stop",
                    "_TelStop",
                    &class_name,
                    Document::Str(", 'reason' => Reason"),
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
        Ok(doc)
    }
}

/// Raise tail of the triple-format noreply arm (BT-4057).
///
/// `exit` with `normal`/`shutdown` is a control signal, not a crash: keep the
/// raw reason so the actor stops normally. Everything else is raised as the
/// wrapped error.
fn noreply_raise_tail(
    type_v: Document<'static>,
    reason_v: Document<'static>,
    stack_v: Document<'static>,
    wrapped_v: Document<'static>,
) -> Document<'static> {
    case_block(
        type_v,
        [
            form::clause(
                [leaf::atom("exit")],
                Some(form::call(
                    "erlang",
                    "or",
                    [
                        form::call("erlang", "=:=", [reason_v.clone(), leaf::atom("normal")]),
                        form::call("erlang", "=:=", [reason_v.clone(), leaf::atom("shutdown")]),
                    ],
                )),
                form::primop("raw_raise", [leaf::atom("exit"), reason_v, stack_v.clone()]),
                Layout::Block,
            ),
            arm(
                [leaf::var("_")],
                form::primop("raw_raise", [leaf::atom("error"), wrapped_v, stack_v]),
            ),
        ],
    )
}

/// Generates the two error case arms shared by every `gen_server` callback
/// that dispatches a message with no reply channel: `handle_cast/2` (`!`
/// sends), a Server's `handle_info/2` (`handleInfo:`) and the
/// `'$bt_continue'` step of `handle_continue/2` (`continueWith:`, which
/// reuses the cast dispatch case).
///
/// ADR 0134 (BT-3958): there is no caller to hand the error to, so the actor
/// crashes the OTP way: both arms raise, `gen_server` calls `terminate/2`
/// with `{Exception, Stacktrace}` as its reason, emits a crash report naming
/// the last message, and the supervisor applies its restart policy.
///
/// Both arms raise the error as a wrapped Exception object (an `error`-class
/// raise of `beamtalk_exception_handler:ensure_wrapped`'s result), so
/// `terminate:` and `onExit:` see the same kind of reason whatever was
/// raised — a Beamtalk `Error signal:`, a raw Erlang `badarith`, an abnormal
/// `exit` or a `throw` — and the same as a runtime-path actor
/// (`beamtalk_actor:dispatch_cast/3`). Raising as `error` also matters for a
/// `throw`: `gen_server` takes a throw escaping a callback as the callback's
/// *return value*, so re-raising it as a throw would install the thrown term
/// as the next state.
///
/// The one exception (BT-4057, ADR 0134 amendment): an `exit` whose reason is
/// `normal` or `shutdown` is a control signal, not a crash, and the triple
/// arm re-raises it as `exit` with the original raw reason so the actor stops
/// normally (see [`noreply_raise_tail`]).
///
/// 1. **Triple-format error** `{'error', {Type, Reason, RawStack}, _State}`:
///    the three-tuple produced by `safe_dispatch`'s `catch`, whose third
///    element is the raw trace the catch clause binds (see
///    `generate_safe_dispatch`). The trace is built for the Exception's
///    `stacktrace` field and the raise keeps the original trace
///    (`primop 'raw_raise'`, since `erlang:raise/3` rejects a raw trace).
/// 2. **Plain-format fallback** `{'error', Error, _State2}`: DNU,
///    `#beamtalk_error{}`, a relayed non-local return, or any non-triple
///    error from dispatch. Raised by `beamtalk_exception_handler:reraise/1`,
///    which the runtime-path actor's `dispatch_cast/3` uses too.
///
/// `var_prefix` is the capitalised variable-name prefix (e.g. `"Cast"` →
/// `CastType`, `CastReason`, …; `"Info"` → `InfoType`, …).  Variable names are
/// produced via [`leaf::var`] to satisfy the typed-leaf rule (ADR 0089).
fn noreply_error_arms(var_prefix: &str) -> [Document<'static>; 2] {
    // Compute variable name strings from the prefix, then pass to leaf::var.
    // format! here derives the *input* to the typed-leaf API, not a raw Core
    // Erlang fragment — this is the sanctioned pattern per ADR 0089.
    let type_v = leaf::var(format!("{var_prefix}Type"));
    let reason_v = leaf::var(format!("{var_prefix}Reason"));
    let stack_v = leaf::var(format!("{var_prefix}Stacktrace"));
    let built_v = leaf::var(format!("{var_prefix}BuiltStack"));
    let wrapped_v = leaf::var(format!("{var_prefix}Exception"));
    let state1_v = leaf::var(format!("_{var_prefix}State"));
    let error_v = leaf::var(format!("{var_prefix}Error"));
    let state2_v = leaf::var(format!("_{var_prefix}State2"));

    [
        // Triple-format arm — wrap, then raise keeping the original trace
        form::clause(
            [form::tuple([
                leaf::atom("error"),
                form::tuple([type_v.clone(), reason_v.clone(), stack_v.clone()]),
                state1_v,
            ])],
            Some(leaf::atom("true")),
            wrap_and_reraise_doc(
                &CatchVars {
                    class: type_v.clone(),
                    reason: reason_v.clone(),
                    stack: stack_v.clone(),
                },
                built_v,
                wrapped_v.clone(),
                None,
                noreply_raise_tail(type_v, reason_v, stack_v, wrapped_v),
                Layout::Block,
            ),
            Layout::Block,
        ),
        // Plain-format fallback — {error, Error, State} (DNU, #beamtalk_error{}, etc.)
        arm(
            [form::tuple([
                leaf::atom("error"),
                error_v.clone(),
                state2_v,
            ])],
            form::call("beamtalk_exception_handler", "reraise", [error_v]),
        ),
    ]
}

#[cfg(test)]
mod tests {
    use crate::core_erlang::CoreErlangGenerator;
    use beamtalk_core::ast::{
        ClassDefinition, Expression, Identifier, Literal, MessageSelector, MethodDefinition,
        Module, StateDeclaration, TypeAnnotation,
    };
    use beamtalk_core::source_analysis::Span;

    fn s() -> Span {
        Span::new(0, 0)
    }

    fn id(name: &str) -> Identifier {
        Identifier::new(name, s())
    }

    fn simple_ta(name: &str) -> TypeAnnotation {
        TypeAnnotation::Simple(id(name))
    }

    fn union_ta(types: Vec<TypeAnnotation>) -> TypeAnnotation {
        TypeAnnotation::Union { types, span: s() }
    }

    fn empty_module() -> Module {
        Module {
            pipeline_sends: beamtalk_core::ast::PipelineSends::default(),
            literal_comments: beamtalk_core::ast::LiteralComments::default(),
            classes: Vec::new(),
            method_definitions: Vec::new(),
            protocols: Vec::new(),
            type_aliases: Vec::new(),
            native_declarations: Vec::new(),
            expressions: Vec::new(),
            span: s(),
            file_leading_comments: Vec::new(),
            file_trailing_comments: Vec::new(),
        }
    }

    fn module_with_class(class: ClassDefinition) -> Module {
        Module {
            classes: vec![class],
            ..empty_module()
        }
    }

    // Nilability itself (ADR 0124 §7 A1) is now a `beamtalk-core` predicate —
    // `beamtalk_core::semantic_analysis::requires_definite_assignment` /
    // `is_nilable_type_name`, unit-tested directly in
    // `crates/beamtalk-core/src/semantic_analysis/definite_assignment.rs`
    // (plain non-nilable, `| Nil`, `| UndefinedObject`, alias-to-nilable,
    // alias-to-non-nilable, intersection, negation, `late`). What remains
    // here is codegen's own wiring: `inherited_typed_no_default_fields`
    // actually calls the shared predicate for both the AST path and the
    // cross-file `ClassInfo` path — see the `inherited_typed_no_default_fields`
    // tests below.

    // --- user_defined_initialize_chain (no-hierarchy fallback) ---

    fn class_with_initialize(name: &str) -> ClassDefinition {
        let init_method = MethodDefinition::new(
            MessageSelector::Unary("initialize".into()),
            Vec::new(),
            Vec::new(),
            s(),
        );
        ClassDefinition::new(id(name), id("Actor"), Vec::new(), vec![init_method], s())
    }

    fn class_without_initialize(name: &str) -> ClassDefinition {
        ClassDefinition::new(id(name), id("Actor"), Vec::new(), Vec::new(), s())
    }

    #[test]
    fn user_defined_initialize_chain_no_hierarchy_with_initialize() {
        let generator = CoreErlangGenerator::new("my_actor");
        let module = module_with_class(class_with_initialize("MyActor"));
        let chain = generator.user_defined_initialize_chain(&module, "MyActor");
        assert_eq!(chain.len(), 1);
        assert_eq!(
            chain[0].module_name,
            generator.compiled_module_name("MyActor")
        );
    }

    #[test]
    fn user_defined_initialize_chain_no_hierarchy_without_initialize() {
        let generator = CoreErlangGenerator::new("my_actor");
        let module = module_with_class(class_without_initialize("MyActor"));
        let chain = generator.user_defined_initialize_chain(&module, "MyActor");
        assert!(chain.is_empty());
    }

    #[test]
    fn user_defined_initialize_chain_no_hierarchy_unknown_leaf_class() {
        let generator = CoreErlangGenerator::new("my_actor");
        let module = module_with_class(class_with_initialize("MyActor"));
        let chain = generator.user_defined_initialize_chain(&module, "UnknownClass");
        assert!(chain.is_empty());
    }

    // --- class_or_ancestor_overrides_terminate (BT-3596, no-hierarchy fallback) ---

    fn class_with_terminate(name: &str) -> ClassDefinition {
        let terminate_method = MethodDefinition::new(
            MessageSelector::Keyword(vec![beamtalk_core::ast::KeywordPart::new(
                "terminate:",
                s(),
            )]),
            Vec::new(),
            Vec::new(),
            s(),
        );
        ClassDefinition::new(
            id(name),
            id("Actor"),
            Vec::new(),
            vec![terminate_method],
            s(),
        )
    }

    #[test]
    fn class_or_ancestor_overrides_terminate_no_hierarchy_with_override() {
        let generator = CoreErlangGenerator::new("my_actor");
        let module = module_with_class(class_with_terminate("MyActor"));
        assert!(generator.class_or_ancestor_overrides_terminate(&module, "MyActor"));
    }

    #[test]
    fn class_or_ancestor_overrides_terminate_no_hierarchy_without_override() {
        let generator = CoreErlangGenerator::new("my_actor");
        let module = module_with_class(class_without_initialize("MyActor"));
        assert!(!generator.class_or_ancestor_overrides_terminate(&module, "MyActor"));
    }

    #[test]
    fn class_or_ancestor_overrides_terminate_no_hierarchy_unknown_leaf_class() {
        let generator = CoreErlangGenerator::new("my_actor");
        let module = module_with_class(class_with_terminate("MyActor"));
        assert!(!generator.class_or_ancestor_overrides_terminate(&module, "UnknownClass"));
    }

    // --- inherited_typed_no_default_fields (no-hierarchy fallback) ---

    #[test]
    fn inherited_typed_no_default_fields_typed_no_default_returned() {
        let field = StateDeclaration::with_type(id("count"), simple_ta("Integer"), s());
        let class = ClassDefinition::new(id("MyActor"), id("Actor"), vec![field], Vec::new(), s());
        let generator = CoreErlangGenerator::new("my_actor");
        let module = module_with_class(class);
        let fields = generator.inherited_typed_no_default_fields(&module, "MyActor");
        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].field_name, "count");
        assert_eq!(fields[0].type_name, "Integer");
        assert_eq!(fields[0].owning_class, "MyActor");
    }

    #[test]
    fn inherited_typed_no_default_fields_typed_with_default_skipped() {
        let default_val = Expression::Literal(Literal::Integer(0), s());
        let field = StateDeclaration::with_type_and_default(
            id("count"),
            simple_ta("Integer"),
            default_val,
            s(),
        );
        let class = ClassDefinition::new(id("MyActor"), id("Actor"), vec![field], Vec::new(), s());
        let generator = CoreErlangGenerator::new("my_actor");
        let module = module_with_class(class);
        let fields = generator.inherited_typed_no_default_fields(&module, "MyActor");
        assert!(fields.is_empty());
    }

    #[test]
    fn inherited_typed_no_default_fields_nilable_type_skipped() {
        let field = StateDeclaration::with_type(id("opt_val"), simple_ta("Nil"), s());
        let class = ClassDefinition::new(id("MyActor"), id("Actor"), vec![field], Vec::new(), s());
        let generator = CoreErlangGenerator::new("my_actor");
        let module = module_with_class(class);
        let fields = generator.inherited_typed_no_default_fields(&module, "MyActor");
        assert!(fields.is_empty());
    }

    #[test]
    fn inherited_typed_no_default_fields_undefined_object_union_skipped() {
        // ADR 0124 §7 A1: the previous codegen-local predicate only matched
        // the literal `Nil` spelling, so `T | UndefinedObject` (the canonical
        // nil class) was wrongly treated as non-nilable. The shared
        // `beamtalk-core` predicate this now delegates to recognises both.
        let field = StateDeclaration::with_type(
            id("opt_val"),
            union_ta(vec![simple_ta("String"), simple_ta("UndefinedObject")]),
            s(),
        );
        let class = ClassDefinition::new(id("MyActor"), id("Actor"), vec![field], Vec::new(), s());
        let generator = CoreErlangGenerator::new("my_actor");
        let module = module_with_class(class);
        let fields = generator.inherited_typed_no_default_fields(&module, "MyActor");
        assert!(fields.is_empty());
    }

    #[test]
    fn inherited_typed_no_default_fields_untyped_skipped() {
        let field = StateDeclaration::new(id("raw"), s());
        let class = ClassDefinition::new(id("MyActor"), id("Actor"), vec![field], Vec::new(), s());
        let generator = CoreErlangGenerator::new("my_actor");
        let module = module_with_class(class);
        let fields = generator.inherited_typed_no_default_fields(&module, "MyActor");
        assert!(fields.is_empty());
    }

    #[test]
    fn inherited_typed_no_default_fields_unknown_class_returns_empty() {
        let field = StateDeclaration::with_type(id("count"), simple_ta("Integer"), s());
        let class = ClassDefinition::new(id("MyActor"), id("Actor"), vec![field], Vec::new(), s());
        let generator = CoreErlangGenerator::new("my_actor");
        let module = module_with_class(class);
        let fields = generator.inherited_typed_no_default_fields(&module, "UnknownClass");
        assert!(fields.is_empty());
    }
}
